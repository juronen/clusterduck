open Core.Std
open Async.Std

type t =
  { mutable next : int
  ; pending : (unit -> unit Deferred.t) Int.Table.t
  ; sequencer : Limiter.Sequencer.t
  }

let create ?(first=0) () =
  { next = first
  ; pending = Int.Table.create ()
  ; sequencer = Limiter.Sequencer.create ()
  }
;; 

let enqueue t id task = 
  Hashtbl.set t.pending ~key:id ~data:task;
  let rec run_next () =
    match Hashtbl.find t.pending t.next with
    | None      -> return ()
    | Some task ->
      Hashtbl.remove t.pending t.next; 
      Limiter.Sequencer.enqueue' t.sequencer task ()
      >>= fun result ->
      match result with
      | Raised e -> raise e
      | Aborted  -> failwithf "Task %d aborted by sequencer" t.next ()
      | Ok ()    ->
        t.next <- t.next + 1;
        run_next ()
  in
  run_next ()
;; 
