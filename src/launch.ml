open Core.Std
open Async.Std
open Rpc_parallel_core_deprecated.Std

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
