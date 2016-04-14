open Core.Std

type t = Host_and_port.t Queue.t

(** [create] converts a list of (host * port) to a queue of Host_and_port.t *)
val create : (string * int) list -> Host_and_port.t Queue.t

(** [pop_machine t] attempts to dequeue a machine from t. *)
val pop_machine : 'a Queue.t -> 'a option
