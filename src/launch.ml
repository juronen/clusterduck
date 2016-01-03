open Core.Std
open Async.Std
open Rpc_parallel_core.Std

module type Worker_sig = sig
  
  type worker_arg
  type worker_ret

  val impls_map : unit Rpc.Implementations.t String.Table.t
  val ports_map : int String.Table.t

  val worker_main
    :  ?rpc_max_message_size:int
    -> ?rpc_handshake_timeout:Time.Span.t
    -> ?rpc_heartbeat_config:Rpc.Connection.Heartbeat_config.t
    -> worker_arg
    -> worker_ret Deferred.t

end;;

module Worker_types = struct

  type worker_arg = String.t with bin_io
  type worker_ret = Host_and_port.t with bin_io

end;;

module Worker_shell () = struct

  type worker_arg = String.t with bin_io
  type worker_ret = Host_and_port.t with bin_io

  let impls_map = String.Table.create ()
  let (ports_map : int String.Table.t) = String.Table.create ()

  let worker_main
    ?rpc_max_message_size
    ?rpc_handshake_timeout
    ?rpc_heartbeat_config
    worker_arg =

    let implementations = Hashtbl.find_exn impls_map worker_arg in
      printf "%s will serve on %d\n%!" worker_arg
      (Hashtbl.find_exn ports_map worker_arg);
      Rpc.Connection.serve
        ~implementations
        ?max_message_size:rpc_max_message_size
        ?handshake_timeout:rpc_handshake_timeout
        ?heartbeat_config:rpc_heartbeat_config
        ~initial_connection_state:(fun _ _ -> ())
        ~where_to_listen:(Tcp.on_port (Hashtbl.find_exn ports_map worker_arg))
        ()
      >>| fun serv ->
      Host_and_port.create
      ~host:(Unix.gethostname ())
      ~port:(Tcp.Server.listening_on serv)
  ;;

end;;


module type Spawner_sig = sig

  open Parallel

  type worker_id

  val spawn_worker
    :  ?where : [`Local | `Remote of _ Remote_executable.t]
    -> ?disown : bool
    -> ?env : (string * string) list
    -> ?rpc_max_message_size : int
    -> ?rpc_handshake_timeout : Time.Span.t
    -> ?rpc_heartbeat_config : Rpc.Connection.Heartbeat_config.t
    -> ?connection_timeout : Time.Span.t
    -> ?redirect_stdout : Fd_redirection.t
    -> ?redirect_stderr : Fd_redirection.t
    -> ?cd : string
    -> ?umask : int
    -> Worker_types.worker_arg
    -> on_failure : (Error.t -> unit)
    -> (Worker_types.worker_ret * worker_id) Or_error.t Deferred.t

  val spawn_worker_exn
    :  ?where : [`Local | `Remote of _ Remote_executable.t]
    -> ?disown : bool
    -> ?env : (string * string) list
    -> ?rpc_max_message_size : int
    -> ?rpc_handshake_timeout : Time.Span.t
    -> ?rpc_heartbeat_config : Rpc.Connection.Heartbeat_config.t
    -> ?connection_timeout : Time.Span.t
    -> ?redirect_stdout : Fd_redirection.t
    -> ?redirect_stderr : Fd_redirection.t
    -> ?cd : string
    -> ?umask : int
    -> Worker_types.worker_arg
    -> on_failure : (Error.t -> unit)
    -> (Worker_types.worker_ret * worker_id) Deferred.t

  val kill_worker : worker_id -> unit Or_error.t Deferred.t

  val run
    :  ?rpc_max_message_size : int
    -> ?rpc_handshake_timeout : Time.Span.t
    -> ?rpc_heartbeat_config : Rpc.Connection.Heartbeat_config.t
    -> Command.t
    -> unit

end;;
