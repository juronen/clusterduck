open Core.Std
open Async.Std
open Rpc_parallel_core_deprecated.Std

module Worker_shell : functor () -> sig

  (** Parallel_deprecated.Make takes in a module with a signature
      that includes Worker_types and worker_main as we have below. 
      At the time of application of Parallel.Make, the module
      passed to it must have all the information it needs to 
      spawn workers. This module is used to generate such a
      module, and is a functor due to the necessity of some
      top level data structures: by using a functor instead of
      a regular module, we essentially make all the data private
      within a call of Builder.build_network. *)

  include module type of Worker_types

  (** [implement desc port subworkers] will create an RPC 
      implementation for the worker described by desc, which
      will be hosted on [port]. The implementation will perform
      the computation of the worker, and then relay the result 
      to [subs]. Uncaught exceptions raised dduring the computation
      will be logged, and result in the worker being shut down. *)
  val implement : 
    ('i, 'o) Worker_desc.t 
    -> int 
    -> (string * Host_and_port.t) list 
    -> unit

  (** [worker_main name] selects an RPC implementation using name,
      starts the server and returns the Host_and_port.t it is hosted on. *)
  val worker_main : worker_arg -> worker_ret Deferred.t

end

module type Parallel_sig = sig

  (** The signature of the output from Parallel_deprecated.Make *)

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

end
