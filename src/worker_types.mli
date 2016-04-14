open Core.Std

(** [worker_arg] specifies the type sent to the worker_main
 * of an RPC worker to specify the functionality it should provide. *)
type worker_arg = String.t [@@deriving bin_io]

(** [worker_ret] specifies the type returned by an RPC worker
 * to indicate where the worker is hosted. *)
type worker_ret = Host_and_port.t [@@deriving bin_io]
