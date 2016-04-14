open Core.Std
open Async.Std

(** [one_way_dispatch rpc machine data] sends [data] to [machine]
    over one way rpc to the endpoint defined by [rpc]. *)
val one_way_dispatch :
  'a Rpc.One_way.t 
  -> Host_and_port.t 
  -> 'a 
  -> (unit Or_error.t, Exn.t) Result.t Deferred.t
