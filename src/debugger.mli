open Core.Std
open Async.Std

module Debug_rpc : sig 

  (** A 3-tuple that holds the worker name, message id
      and string-form message sent out from a worker. *)
  type t = (string * int * string) [@@deriving bin_io]

  val rpc : t Rpc.One_way.t

end 

(** [t] stores the master machine, list of workers whose output
    will be debugged, and the RPC implementation that calls the 
    user's function with received data. *)
type t = Host_and_port.t * (string list) * (unit Rpc.Implementations.t)

(** Used internally - starting the server has to be delayed
    until the async scheduler is up, which doesn't happen until
    Spawner.run is called (can't work around this as it is just 
    the way Rpc_parallel_core is built). *)
val start_debug_server : t -> unit Deferred.t

(** [create names f port] creates a debugger that will
    make the output of the workers be sent back to the master,
    such that f is called as such:

      f worker_name msg_id msg  
                                                           
 *)
val create :
  workers:string list
  -> f:(string -> int -> string -> unit)
  -> port:int
  -> t
