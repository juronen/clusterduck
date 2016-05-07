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
 
module Network : sig

  (** A type that separately stores spouts and downstream
      workers, and holds a first class module that has the
      functionality to spawn workers. *)
  type t =
    { spouts  : (string * Host_and_port.t) list
    ; workers : (string * Host_and_port.t) list
    ; spawner : (module Parallel_sig)
    }

end

type t 

(** [create ~machines] creates a Builder.t that
    can be used to build a Clusterduck network. *)
val create : 
  ?debugger:Debugger.t 
  -> (string * int) list -> t

(** [add_worker t desc] adds desc to the network. *)
val add_worker : t -> (('i, 'o) Worker_desc.t) -> unit

(** [launch t network ()] will spawn the workers and
    the spouts, and then send each spout an Rpc message
    to initialize and start them.

    Pass in [cd] to specify the directory where logs are
    placed, along with copies of the executable.
    Defaults to /tmp/clusterduck.

    The function passed in as [on_failure] is called when
    the worker experiences a failure that causes it to shut down. *)
val launch : 
  t 
  -> Network.t 
  -> ?cd:string
  -> ?on_failure:(string -> Error.t -> unit)
  -> unit
  -> unit Deferred.t

(** [build_network t] creates an RPC implementation for each
    Worker_desc.t added through [add_worker]. The dependencies
    of each worker are used to determine a list of subworkers for
    a given worker. Once the worker is done processing, it will send
    its result to each of the subworkers over one-way RPC. *)
val build_network : 
  t 
  -> Network.t

