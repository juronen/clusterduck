open Core.Std
open Async.Std

module Debug_rpc = struct

  type t = (string * int * string) [@@deriving bin_io]

  let rpc = Rpc_util.create_one_way "clusterduck_debug" bin_t

end;;

type t = Host_and_port.t * (string list) * (unit Rpc.Implementations.t)

let start_debug_server (box, _, impl) =
  Rpc.Connection.serve
    ~implementations:impl
    ~initial_connection_state:(fun _ _ -> ())
    ~where_to_listen:(Tcp.on_port (Host_and_port.port box))
    ()
  >>| fun _server -> ()
;;

let create ~workers ~f ~port =
  let implementation =
    Rpc.One_way.implement Debug_rpc.rpc (fun () (name, msg_out, msg_id) ->
      f name msg_out msg_id
    )
  in
  let impl =
    Rpc.Implementations.create_exn
      ~implementations:[implementation]
      ~on_unknown_rpc:`Close_connection
  in
  (Host_and_port.create ~host:(Unix.gethostname ()) ~port, workers, impl)
;;

