open Core.Std
open Async.Std
open Clusterduck

let spout_func = fun () ->
  let rnd = Random.int 10 in
  printf "[Spout] %d\n%!" rnd;
  return rnd
;;

let histogram = Int.Table.create ();;

let count_func = fun num ->
  let updated =
    match Hashtbl.find histogram num with
    | Some count -> count + 1
    | None       -> 1
  in
  Hashtbl.replace histogram ~key:num ~data:updated;
  return ()
;;

let generate_local_cluster () =
  let hosts = List.init 200 ~f:(fun _ -> "localhost") in
  let ports = List.init 200 ~f:(fun x -> 5000 + x) in
  List.zip_exn hosts ports
;;

let printf = Core.Std.printf

let handle_error worker err =
  failwiths (sprintf "error in %s" worker) err Error.sexp_of_t
;;

let () =
  let machines = Machines.create ~machines:(generate_local_cluster()) in
  let builder = Builder.create ~machines:machines in
  let spout_d =
    Worker_desc.create
      ~name:"spout"
      ~input:Unit.bin_t
      ~output:Int.bin_t
      ~deps:[]
      ~f:spout_func
  in 
  let count_d =
    Worker_desc.create
      ~name:"count"
      ~input:Int.bin_t
      ~output:Unit.bin_t
      ~deps:["spout"]
      ~f:count_func
  in
  Builder.add_worker builder spout_d;
  Builder.add_worker builder count_d;
  let {Network.roots; workers; spawner} = Builder.build_network builder in
  List.iter roots ~f:(fun (name, loc) ->
    printf "%s >>> %s\n%!" name (Host_and_port.to_string loc)
  );
  List.iter workers ~f:(fun (name, loc) ->
    printf "%s >>> %s\n%!" name (Host_and_port.to_string loc)
  ); 
  let module Spawner = (val spawner : Launch.Spawner_sig) in
  let command =
    Command.async ~summary:"Spout and count"
      Command.Spec.(empty) 
      (fun () ->
        let launch ls = 
          Deferred.List.map ls ~f:(fun (name, loc) ->
            printf "Spawning %s\n%!" name;
            Spawner.spawn_worker_exn ~where:`Local name
            ~cd:"/tmp/clog"
            ~redirect_stdout:(`File_append (name ^ ".out"))
            ~redirect_stderr:(`File_append (name ^ ".err"))
            ~on_failure:(handle_error name)
          >>| fun (worker, _) -> (name, worker)
          )
        in
        launch workers
        >>= fun _ ->
        launch roots
        >>= fun running_spouts ->
        Deferred.List.iter running_spouts ~f:(fun (name, spout) ->
          let rpc = 
            Rpc.One_way.create
              ~name
              ~version:0
              ~bin_msg:Unit.bin_t
          in
          Rpc.Connection.with_client
            ~host:(Host_and_port.host spout)
            ~port:(Host_and_port.port spout)
            (fun conn -> return (Rpc.One_way.dispatch rpc conn ()))
          >>| function
          | Error e -> failwiths "Conn fail" e Exn.sexp_of_t
          | Ok (Error e) -> failwiths "Dispatch fail" e Error.sexp_of_t
          | Ok (Ok ()) -> printf "Woot\n%!"
        )
      )
  in
  Spawner.run command

