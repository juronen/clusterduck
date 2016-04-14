open Core.Std
open Async.Std

module Func_container : sig

  type ('input, 'output) func_type =
    [ `Spout of 'input -> (int -> 'output -> unit Deferred.t) -> unit Deferred.t
    | `Worker of 'input -> 'output Deferred.t ]

  type ('input, 'output) t =
    [ `Simple of ('input, 'output) func_type
    | `Bundled of
      ('input, 'output Deferred.t) Bundler.Dyn_application.func_type *
        (int -> string -> 'input -> 'input Bundler.Fill_result.t)]
    
end

type ('input, 'output) t = 
  { name : string
  ; input : (int * 'input) Bin_prot.Type_class.t0
  ; output : (int * 'output) Bin_prot.Type_class.t0
  ; deps : string list
  ; init : 'input option
  ; func : ('input, 'output) Func_container.t
  ; sequencer : Ordered_sequencer.t option
  }

module type Worker_IO = sig

  type input [@@deriving bin_io, sexp]
  type output [@@deriving bin_io, sexp]

end

(** [create_simple] returns a t that describes a worker which
    may either be a spout (data source), or a worker that depends
    on spouts or other workers for its inputs. *)
val create_simple :
  (module Worker_IO with type input = 'a and type output = 'b) 
  -> name:string 
  -> ?deps:string list 
  -> ?init:'a 
  -> ('a, 'b) Func_container.func_type 
  -> ('a, 'b) t

(** [create_bundled] is used to create a worker that depends on multiple
    upstream workers, which together produce batches (bundles) of
    messages which this worker consumes. *)
val create_bundled :
  (module Worker_IO with type input = 'a and type output = 'b) 
  -> name:string 
  -> ?deps:string list 
  -> ?sequencer_start:int 
  -> ('a, 'b Deferred.t) Bundler.Dyn_application.func_type 
  -> ('a, 'b) t