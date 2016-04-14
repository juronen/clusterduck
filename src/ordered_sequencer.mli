(** A constrained sequencer that provides two guarantees:
    
      1. At most one task is being executed at any time. (Limiter.Sequencer)
      2. Tasks are executed in the order determined by their associated ids.
         That is, a task that was enqueued with id n will only be executed
         after the task with n - 1 has finished, or if n is the starting id
         passed to [create]. *)

open Core.Std
open Async.Std

type t

(** [create ~first:n ()] creates a new sequencer where the task ids
    count up from [first], which has a default value of 0. *)
val create : ?first:int -> unit -> t

(** [enqueue t id task] will place task into a table of pending tasks,
    and executes it if id = t.next. As many subsequent tasks as possible
    are ran - if the table contains ids 2, 3, 4 and t.next is 1, when
    a task with id 1 is enqueued all of the will be executed in order. *)
val enqueue :
  t 
  -> int 
  -> (unit -> unit Deferred.t) 
  -> unit Deferred.t
