open Core.Std
open Async.Std

(** [one_way_dispatch rpc machine data] sends [data] to [machine]
    over one way rpc to the endpoint defined by [rpc]. *)
val one_way_dispatch :
  'a Rpc.One_way.t 
  -> Host_and_port.t 
  -> 'a 
  -> (unit Or_error.t, Exn.t) Result.t Deferred.t
 
(** [create_one_way name bin_msg] unlabeled helper for
    Rpc.One_way.create to hide away the clunky call. *)
val create_one_way : 
  string 
  -> 'a Bin_prot.Type_class.t 
  -> 'a Rpc.One_way.t

(** [match_error e] unpacks the nested result/error from one_way_dispatch.*)
val match_error : (unit Or_error.t, Exn.t) Result.t  -> string * string
  
