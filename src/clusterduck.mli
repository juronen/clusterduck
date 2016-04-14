open Core.Std
open Async.Std
open Rpc_parallel_core_deprecated.Std

module Network : sig

  (** A type that separately stores spouts and downstream
      workers, and holds a first class module that has the
      functionality to spawn workers. *)
  type t =
    { spouts  : (string * Host_and_port.t) list
    ; workers : (string * Host_and_port.t) list
    ; spawner : (module Launch.Parallel_sig)
    }

end

module Builder : sig

  module Existential : sig

    (** This type hides the parameters of a Worker_desc.t,
        which allows us to store descs of multiple parametrizations
        in a container. *)
    type t = Any : ('i, 'o) Worker_desc.t -> t

  end

  type t = 
    { descs    : Existential.t String.Table.t
    ; machines : Host_and_port.t Queue.t
    }


  (** [create ~machines] creates a Builder.t that
      can be used to build a Clusterduck network. *)
  val create : machines:(string * int) list -> t

  (** [add_worker t desc] adds desc to the network. *)
  val add_worker : t -> (('i, 'o) Worker_desc.t) -> unit

  (** [launch t network ()] will spawn the workers and
      the spouts, and then send each spout an Rpc message
      to initialize and start them.

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
  val build_network : t -> Network.t

end
