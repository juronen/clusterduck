open Core.Std

type worker_arg = String.t [@@deriving bin_io]
type worker_ret = Host_and_port.t [@@deriving bin_io]
