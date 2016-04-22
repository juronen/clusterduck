open Core.Std
open Async.Std
open Rpc_parallel_core_deprecated.Std

module Network = struct

  type t =
    { spouts   : (string * Host_and_port.t) list
    ; workers  : (string * Host_and_port.t) list
    ; spawner  : (module Launch.Parallel_sig)
    } 

end;;

module Builder = struct
 
  module Any_worker = struct

    (** Hide the type parameterization of a worker so we can store
        them in a single container. *)
    type t = Any : ('i, 'o) Worker_desc.t -> t

  end;;

  type t =
    { descs    : Any_worker.t String.Table.t
    ; debugger : Debugger.t option
    ; machines : Host_and_port.t Queue.t
    }

  let create ?debugger ~machines =
    let machines =
      List.map machines ~f:(fun (host, port) -> Host_and_port.create ~host ~port)
      |> Queue.of_list
    in
    { descs = String.Table.create ()
    ; debugger
    ; machines
    }
  ;;

  let add_worker t (desc : ('i, 'o) Worker_desc.t) =
    Hashtbl.set t.descs ~key:desc.name ~data:(Any_worker.Any desc)
  ;;

  let build_subs_map t = 
    let sub_tbl = String.Table.create () in
    Hashtbl.iteri t.descs ~f:(fun ~key:worker ~data:(Any_worker.Any desc) ->
      List.iter desc.deps ~f:(fun parent_worker ->
        let updated_subs =
          match Hashtbl.find sub_tbl parent_worker with
          | None      -> worker :: []
          | Some subs -> worker :: subs
        in
        Hashtbl.set sub_tbl ~key:parent_worker ~data:updated_subs
      )
    );
    sub_tbl
  ;;

  let assign_machines t =
    Hashtbl.mapi t.descs ~f:(fun ~key:name ~data:_ ->
      match Queue.dequeue t.machines with
      | Some machine -> machine
      | None         -> failwithf "Out of machines for worker %s\n%!" name ()
    )
  ;;

  let create_dispatcher (desc: _ Worker_desc.t) subs_map ?debug_box () =
    match Hashtbl.find subs_map desc.name with
    | None      -> fun _id _msg -> return ()
    | Some subs -> 
      fun id msg ->
        let disconnect child = 
          let remaining = 
            List.filter subs ~f:(fun (sub, _machine) ->
              not (String.equal sub child)
            ) 
          in
          Hashtbl.set subs_map ~key:desc.name ~data:remaining
        in
        begin
          match debug_box with
          | None            -> return ()
          | Some box -> 
            Rpc_util.one_way_dispatch 
              Debugger.Debug_rpc.rpc 
              box 
              (desc.name, id, desc.str_of_out msg)
            >>| fun _ -> ()
        end 
        >>= fun () ->
        Deferred.List.iter subs ~f:(fun (sub_name, machine) ->
          let rpc = Rpc_util.create_one_way desc.name desc.output in
          Rpc_util.one_way_dispatch rpc machine (id, msg)
          >>| function
          | Ok (Ok ()) -> ()
          | error      ->
            disconnect sub_name;
            let (err_msg, fail_type) = Rpc_util.match_error error in
            eprintf 
              "%s failure: [%s] from %s to %s" 
               fail_type
               err_msg
               desc.name
               sub_name 
        )
  ;;

  let create_basic_impl (desc : _ Worker_desc.t) rpc_name dispatcher f =
    let rpc = Rpc_util.create_one_way rpc_name desc.input in
    let impl = Rpc.One_way.implement rpc (fun () (id, msg) ->
      let run () =
        match f with
        | `Spout spout   -> spout msg dispatcher
        | `Worker worker -> 
          worker msg 
          >>= fun result -> 
          dispatcher id result
      in
      Monitor.try_with ~rest:`Log run
      >>> function
      | Ok ()   -> ()
      | Error e -> 
        eprintf "Unhandled exception in worker: %s\n%!" (Exn.to_string e);
        shutdown 0
    )
    in
    impl
  ;;

  let create_basic_impls desc dispatcher func =
    match func with
    | `Spout  _ -> [create_basic_impl desc desc.name dispatcher func]
    | `Worker _ ->
      List.map desc.deps ~f:(fun dep_name ->
        create_basic_impl desc dep_name dispatcher func
      )
  ;;

  let create_bundled_impls (desc: _ Worker_desc.t) dispatcher f bundler =
    List.fold desc.deps ~init:[] ~f:(fun acc dep_name ->
      let rpc = Rpc_util.create_one_way dep_name desc.input in 
      let impl = Rpc.One_way.implement rpc (fun () (id, msg) ->
        match bundler id dep_name msg with
        | `Active     -> ()
        | `New bundle ->
          let on_bundle_complete () =
            bundle
            >>= fun args_list ->
            Monitor.try_with ~rest:`Log (fun () ->
              Bundler.Dyn_application.apply f args_list
              >>= fun result -> 
              dispatcher id result
            )
            >>| function
            | Ok ()   -> ()
            | Error e -> 
              eprintf "Unhandled exception in worker: %s\n%!" (Exn.to_string e);
              shutdown 0
          in
          match desc.sequencer with
          | None     -> don't_wait_for(on_bundle_complete ())
          | Some seq -> 
            Ordered_sequencer.enqueue seq id on_bundle_complete
            |> don't_wait_for
      )
      in
      impl :: acc
    )
  ;;

  let worker_main impls_map find_machine name =
    let implementations = Hashtbl.find_exn impls_map name in
    let port = find_machine name |> Host_and_port.port in
    Rpc.Connection.serve
      ~implementations 
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:(Tcp.on_port port)
      ()
    >>| fun _server ->
    Host_and_port.create ~host:(Unix.gethostname ()) ~port
  ;;

  let build_network t () =
    let machine_map = assign_machines t in
    let find_machine = Hashtbl.find_exn machine_map in
    let subworker_map = 
      Hashtbl.mapi (build_subs_map t) ~f:(fun ~key ~data:sub_names ->
        List.map sub_names ~f:(fun name -> (name, find_machine name))
      )
    in
    let implementation_map = 
      Hashtbl.mapi t.descs ~f:(fun ~key ~data:(Any_worker.Any desc) ->
        let dispatcher =
          match t.debugger with 
          | None                         ->
            create_dispatcher desc subworker_map ()
          | Some (debug_box, workers, _) ->
            if List.mem workers key
            then
              create_dispatcher desc subworker_map ~debug_box ()
            else
              create_dispatcher desc subworker_map ()
        in
        let implementations =
          match desc.func with
          | `Simple f             -> create_basic_impls desc dispatcher f
          | `Bundled (f, bundler) -> 
            create_bundled_impls desc dispatcher f bundler
        in 
        Rpc.Implementations.create_exn
          ~implementations
          ~on_unknown_rpc:`Close_connection
      )
    in
    (* Separate the spout workers from the others,
       as they must be started last and require initialization. *)
    let (spouts_map, workers_map) = 
      Hashtbl.partitioni_tf machine_map ~f:(fun ~key ~data ->
        Hashtbl.mem subworker_map key
      )
    in
    let spouts = Hashtbl.to_alist spouts_map in
    let workers = Hashtbl.to_alist workers_map in
    let module Spawner = 
      Parallel_deprecated.Make(
        struct
          include Worker_types
          let worker_main = worker_main implementation_map find_machine
        end
      ) 
    in
    Network.
      { spouts
      ; workers
      ; spawner = (module Spawner : Launch.Parallel_sig)
      }
  ;;

  let default_on_failure name err =
    printf "%s: %s\n%!" name (Error.to_string_hum err)
  ;;

  let launch t (network : Network.t)
    ?(cd="/tmp/clusterduck")
    ?(on_failure=default_on_failure) () =
    begin
      match t.debugger with
      | None          -> return ()
      | Some debugger -> Debugger.start_debug_server debugger
    end
    >>= fun _ -> 
    let module Spawner = (val network.spawner : Launch.Parallel_sig) in
    let launch workers = 
      Deferred.List.map workers ~f:(fun (name, machine) ->
        printf "Spawning %s\n%!" name;
        let redirect_stdout = `File_append (name ^ ".out") in
        let redirect_stderr = `File_append (name ^ ".err") in 
        let host = Host_and_port.host machine in
        Parallel_deprecated.Remote_executable.copy_to_host
          ~executable_dir:cd
          ~strict_host_key_checking:`No
          host
        >>= function
        | Error e   -> 
          failwithf "Error copying to %s: %s" host (Error.to_string_hum e) ()
        | Ok remote ->
          Spawner.spawn_worker_exn name
            ~where: (`Remote remote)
            ~cd
            ~redirect_stdout
            ~redirect_stderr
            ~on_failure:(on_failure name)
          >>| fun (host_and_port, _id) -> 
          (name, host_and_port)
      )
    in
    launch network.workers 
    >>= fun _workers ->
    launch network.spouts
    >>= fun running_spouts ->
    Deferred.List.iter running_spouts ~f:(fun (name, machine) ->
      let (Any_worker.Any desc) = Hashtbl.find_exn t.descs name in
      match desc.init with
      | None      -> failwithf "%s: Spout has no initializer" name ()
      | Some init ->
        let rpc = Rpc_util.create_one_way name desc.input in
        Rpc_util.one_way_dispatch rpc machine (0, init)
         >>| function
         | Ok (Ok ()) -> printf "%s: Successfully started spout\n%!" name
         | error      ->
           let (err_msg, fail_type) = Rpc_util.match_error error in
           failwithf "%s: %s failure: %s" name fail_type err_msg ()
    )
    >>= fun () ->
    Deferred.never ()
  ;;

end;;
