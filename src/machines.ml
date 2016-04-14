open Core.Std
open Async.Std

type t = Host_and_port.t Queue.t

let create machines =
  List.map machines ~f:(fun (host, port) -> Host_and_port.create ~host ~port)
  |> Queue.of_list
;;

let pop_machine t = Queue.dequeue t
