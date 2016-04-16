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

    (** Hide the type parametrization of a worker so we can store
        them in a single container. *)
    type t = Any : ('i, 'o) Worker_desc.t -> t

  end;;

  type t =
    { descs    : Any_worker.t String.Table.t
    ; machines : Host_and_port.t Queue.t
    }

  let create ~machines =
    let machines =
      List.map machines ~f:(fun (host, port) -> Host_and_port.create ~host ~port)
      |> Queue.of_list
    in
    { descs = String.Table.create ()
    ; machines
    }
  ;;

  let add_worker t (desc : ('i, 'o) Worker_desc.t) =
    Hashtbl.set t.descs ~key:desc.name ~data:(Any_worker.Any desc)
  ;;

  (** I figured that for the user it's more intuitive to reason about
      the dependencies that each worker has, rather than all the
      sub workers that a worker has to push to, so here we invert this
      relationship in order to tell each worker where to send its results. *)
  let build_subs_map t = 
    let sub_tbl = String.Table.create () in
    Hashtbl.iteri t.descs ~f:(fun ~key:worker_name ~data ->
      let (Any_worker.Any desc) = data in
      List.iter desc.deps ~f:(fun parent_worker ->
        let updated_subs =
          match Hashtbl.find sub_tbl parent_worker with
          | None      -> worker_name :: []
          | Some subs -> worker_name :: subs
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

  let default_on_failure name err =
    printf "%s %s\n%!" name (Error.to_string_hum err)
  ;;

  let launch t (network : Network.t)
    ?(cd="/tmp/clusterduck")
    ?(on_failure=default_on_failure) () =
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
      | None      -> failwithf "%s: Spout must be initialized" name ()
      | Some init ->
        let rpc =
          Rpc.One_way.create
            ~name
            ~version:0
            ~bin_msg:desc.input
        in
        Rpc_util.one_way_dispatch rpc machine (0, init)
         >>| function
         | Ok (Ok ())   ->
           printf "%s: Succesfully initialized spout\n%!" name
         | Error e      ->
           failwithf "%s: Connection failure: %s" name (Exn.to_string e) ()
         | Ok (Error e) ->
           failwithf "%s: Dispatch failure: %s" name (Error.to_string_hum e) ()

    )
    >>= fun () ->
    Deferred.never ()
  ;;

  let build_network t =
    let module Worker_shell = Launch.Worker_shell () in
    let machine_map = assign_machines t in
    let find_machine = Hashtbl.find_exn machine_map in
    let subworker_map = build_subs_map t in
    Hashtbl.iteri t.descs ~f:(fun ~key:name ~data:(Any_worker.Any desc) ->
      let sub_workers =
        Hashtbl.find subworker_map name
        |> Option.value ~default:[]
        |> List.map ~f:(fun sub -> (sub, find_machine sub))
      in
      let port = find_machine name |> Host_and_port.port in
      Worker_shell.implement desc port sub_workers
    );
    (* Separate the spout workers from the others,
       as they must be started last and require initialization. *)
    let (spouts_map, workers_map) = 
      Hashtbl.partitioni_tf machine_map ~f:(fun ~key ~data:_ ->
        Hashtbl.mem subworker_map key
      )
    in
    let spouts = Hashtbl.to_alist spouts_map in
    let workers = Hashtbl.to_alist workers_map in
    let module Spawner = Parallel_deprecated.Make(Worker_shell) in
    Network.
      { spouts
      ; workers
      ; spawner = (module Spawner : Launch.Parallel_sig)
      }
  ;;

 end;;
