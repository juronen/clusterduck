open Core.Std
open Async.Std

let one_way_dispatch rpc machine data =
  Rpc.Connection.with_client
    ~host:(Host_and_port.host machine)
    ~port:(Host_and_port.port machine)
    (fun conn -> return (Rpc.One_way.dispatch rpc conn data))
;;

let create_one_way name bin_msg =
  Rpc.One_way.create
    ~name
    ~version:0
    ~bin_msg
;;

let match_error e =
  match e with
  | Ok (Error e) -> (Error.to_string_hum e, "Dispatch")
  | Error e      -> (Error.of_exn e |> Error.to_string_hum, "Connection")
;;


