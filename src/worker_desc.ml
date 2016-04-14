open Core.Std
open Async.Std

module Func_container = struct

  type ('input, 'output) func_type =
    [ `Spout of ('input -> (int -> 'output -> unit Deferred.t) -> unit Deferred.t)
    | `Worker of ('input -> 'output Deferred.t)]

  type ('input, 'output) t =
    [ `Simple of ('input, 'output) func_type
    | `Bundled of
      ('input, 'output Deferred.t) Bundler.Dyn_application.func_type *
        (int -> string -> 'input -> 'input Bundler.Fill_result.t)]

end;;

type ('input, 'output) t =
  { name      : string 
  ; input     : (int * 'input) Bin_prot.Type_class.t
  ; output    : (int * 'output) Bin_prot.Type_class.t
  ; deps      : string list
  ; init      : 'input option
  ; func      : ('input, 'output) Func_container.t
  ; sequencer : Ordered_sequencer.t option
  }

module type Worker_IO = sig

  type input [@@deriving bin_io, sexp]
  type output [@@deriving bin_io, sexp]

end;;

module Bin_with_int (M : Worker_IO) = struct

  type input = (Int.t * M.input) [@@deriving bin_io, sexp]
  type output = (Int.t * M.output) [@@deriving bin_io, sexp]

end;;

let create_simple 
  (type i)
  (type o)
  (module T : Worker_IO with type input = i and type output = o)
  ~name ?(deps=[]) ?init f =
  let module Bin_io = Bin_with_int(T) in
  { name
  ; input = Bin_io.bin_input
  ; output = Bin_io.bin_output
  ; deps
  ; init
  ; func = `Simple f 
  ; sequencer = None
  }
;;

let create_bundled
  (type i) 
  (type o)
  (module T : Worker_IO with type input = i and type output = o)
  ~name ?(deps=[]) ?sequencer_start f =
  let module Bin_io = Bin_with_int(T) in
  let module Bundler = Bundler.Make(struct type t = i end) in
  let bundler = Bundler.update (Bundler.create deps) in
  let sequencer = 
    Option.map sequencer_start 
    ~f:(fun first -> Ordered_sequencer.create ~first ())
  in
  { name
  ; input = Bin_io.bin_input
  ; output = Bin_io.bin_output
  ; deps
  ; init = None
  ; func = `Bundled (f, bundler)
  ; sequencer
  }
;; 
  
