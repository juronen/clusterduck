open Core.Std
open Async.Std

let one_way_dispatch rpc machine data =
  Rpc.Connection.with_client
    ~host:(Host_and_port.host machine)
    ~port:(Host_and_port.port machine)
    (fun conn -> return (Rpc.One_way.dispatch rpc conn data))
;;
