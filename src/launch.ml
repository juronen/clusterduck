open Core.Std
open Async.Std
open Rpc_parallel_core_deprecated.Std

module Worker_shell () = struct

  include Worker_types

  type subworker_list = (string * Host_and_port.t) list

  let impls_map = String.Table.create ()

  let ports_map = String.Table.create ()

  let subs_map  = String.Table.create ()

  let print_rpc_error e =
    let (error_msg, failure_type) =
      match e with
      | Ok (Error e) -> (Error.to_string_hum e, "Dispatch")
      | Error e      -> (Error.of_exn e |> Error.to_string_hum, "Connection")
    in
    eprintf "%s failure: [%s] from %s to %s" failure_type error_msg
  ;;

  let create_dispatcher (desc: ('i, 'o) Worker_desc.t) =
    match Hashtbl.find subs_map desc.name with
    | None      -> fun _id _msg -> return ()
    | Some subs -> 
      fun id msg ->
        let unsubscribe remove_name = 
          let remaining = 
            List.filter subs ~f:(fun (sub_name, _machine) ->
              not (String.equal sub_name remove_name)
            ) 
          in
          Hashtbl.set subs_map ~key:desc.name ~data:remaining
        in
        Deferred.List.iter subs ~f:(fun (sub_name, machine) ->
          let rpc = Rpc.One_way.create
            ~name:desc.name
            ~version:0
            ~bin_msg:desc.output
          in
          Rpc_util.one_way_dispatch rpc machine (id, msg)
          >>| function
          | Ok (Ok ())                    -> ()
          | (Error _ | Ok (Error _)) as e ->
            unsubscribe sub_name;
            print_rpc_error e desc.name sub_name
        )
  ;;

  (* No bundler, no sequencer *)
  let create_basic_impl (desc : ('i, 'o) Worker_desc.t) rpc_name dispatcher f =
    let rpc =
      Rpc.One_way.create
        ~name:rpc_name
        ~version:0
        ~bin_msg:desc.input
    in
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

  let create_basic_impls desc func =
    let dispatcher = create_dispatcher desc in
    match func with
    | `Spout  _ -> [create_basic_impl desc desc.name dispatcher func]
    | `Worker _ ->
      List.map desc.deps ~f:(fun dep_name ->
        create_basic_impl desc dep_name dispatcher func
      )
  ;;

  let create_bundled_impls desc (f, bundler) =
    let dispatcher = create_dispatcher desc in
    (* For every dependency, create an Rpc implementation that
       will pass the message to the bundler, and run f when
       the bundle is complete. *)
    List.fold desc.deps ~init:[] ~f:(fun acc dep_name ->
      let rpc = 
        Rpc.One_way.create 
          ~name:dep_name 
          ~version:0 
          ~bin_msg:desc.input
      in
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

  let implement (desc: ('i, 'o) Worker_desc.t) port subs =
    Hashtbl.set subs_map  ~key:desc.name ~data:subs;
    Hashtbl.set ports_map ~key:desc.name ~data:port;
    let impls =
      match desc.func with
      | `Simple f       -> create_basic_impls desc f
      | `Bundled bundle -> create_bundled_impls desc bundle
    in 
    let rpc_impls = 
      Rpc.Implementations.create_exn 
        ~implementations:impls
        ~on_unknown_rpc:`Close_connection
    in
    Hashtbl.set impls_map ~key:desc.name ~data:rpc_impls;
  ;;

  let worker_main worker_arg =
    let implementations = Hashtbl.find_exn impls_map worker_arg in
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:(Tcp.on_port (Hashtbl.find_exn ports_map worker_arg))
      ()
    >>| fun serv ->
    Host_and_port.create
    ~host:(Unix.gethostname ())
    ~port:(Tcp.Server.listening_on serv)
  ;;

end;;

module type Parallel_sig = sig

  open Parallel_deprecated
  open Worker_types

  type worker_id

  val spawn_worker
    :  ?where : [`Local | `Remote of _ Remote_executable.t]
    -> ?disown : bool
    -> ?env : (string * string) list
    -> ?rpc_max_message_size : int
    -> ?rpc_handshake_timeout : Time.Span.t
    -> ?rpc_heartbeat_config : Rpc.Connection.Heartbeat_config.t
    -> ?connection_timeout : Time.Span.t
    -> ?cd : string
    -> ?umask : int
    -> redirect_stdout : Fd_redirection.t
    -> redirect_stderr : Fd_redirection.t
    -> worker_arg
    -> on_failure : (Error.t -> unit)
    -> (worker_ret * worker_id) Or_error.t Deferred.t

  val spawn_worker_exn
    :  ?where : [`Local | `Remote of _ Remote_executable.t]
    -> ?disown : bool
    -> ?env : (string * string) list
    -> ?rpc_max_message_size : int
    -> ?rpc_handshake_timeout : Time.Span.t
    -> ?rpc_heartbeat_config : Rpc.Connection.Heartbeat_config.t
    -> ?connection_timeout : Time.Span.t
    -> ?cd : string
    -> ?umask : int
    -> redirect_stdout : Fd_redirection.t
    -> redirect_stderr : Fd_redirection.t
    -> worker_arg
    -> on_failure : (Error.t -> unit)
    -> (worker_ret * worker_id) Deferred.t

  val kill_worker : worker_id -> unit Or_error.t Deferred.t

  val run
    :  ?rpc_max_message_size : int
    -> ?rpc_handshake_timeout : Time.Span.t
    -> ?rpc_heartbeat_config : Rpc.Connection.Heartbeat_config.t
    -> ?where_to_listen:Tcp.Where_to_listen.inet
    -> Command.t
    -> unit

end;;
