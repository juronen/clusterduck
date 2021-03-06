open Core.Std
open Async.Std

module Dyn_application : sig

  (** This module provides a method for calling functions
      of a variable number of arguments with an argument list.
      I stopped at 5 args here, but if there's some convenient type gymnastics
      that can be done to generate these I'd be interested, although
      availability of a higher number of args comes with a clear
      diminishing utility. *)

  type ('a, 'b) func_type =
    [ `A1 of 'a -> 'b 
    | `A2 of 'a -> 'a -> 'b
    | `A3 of 'a -> 'a -> 'a -> 'b 
    | `A4 of 'a -> 'a -> 'a -> 'a -> 'b 
    | `A5 of 'a -> 'a -> 'a -> 'a -> 'a -> 'b ]

  val apply :
    ('a, 'b Deferred.t) func_type
    -> 'a list 
    -> 'b Deferred.t

end

module Fill_result : sig

  type 'a t = [`Active | `New of 'a list Deferred.t]

end

module Make : functor (D : sig type t end) -> sig

  (** This functor returns a Bundler, which can be used to group (bundle)
      together values that are "pushed" asynchronously from some source.

      Clusterduck uses a Bundler to group together messages that arrive from
      multiple workers over RPC - messages belonging in the same group
      share the same sequence id, and are distinguished from each other by
      a key, which in the case of Clusterduck corresponds to the name of the
      upstream worker that the message was received from.

      Internally, a bundle holds a table of keys and Ivars, which are filled
      with values corresponding to the keys. Once the Ivar for each key is
      filled for a certain sequence id, the bundle is "complete":
      the first call to [update] returns `New 'a list Deferred.t,
      where the deferred list holds the values in the same order as the
      keys were in when passed to [create]. Subsequent calls to [update]
      will just return `Active. *)

  module Bundle : sig

    type t 

  end

  type t = 
    { keys : string list
    ; bundles : (int, Bundle.t) Hashtbl.t
    }

  (** [create] creates a bundler using a list of keys. *) 
  val create : string list -> t

  (** [update] sets a value corresponding to a key for this sequence id. *)
  val update : t -> int -> string -> D.t -> D.t Fill_result.t

end
