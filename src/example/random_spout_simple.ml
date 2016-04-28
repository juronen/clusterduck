open Core.Std
open Async.Std
open Std

module Simple_test = struct

  module Spout = struct
    type input = unit [@@deriving bin_io, sexp]
    type output = int [@@deriving bin_io, sexp]
  end

  module Subworker = struct
    type input = int [@@deriving bin_io, sexp]
    type output = unit [@@deriving bin_io, sexp]
  end

  let spout_func = fun () dispatcher ->
    let id = ref 0 in
    Clock.every' 
      (Core.Span.of_sec 1.0) 
      (fun () -> 
        let rnd = Random.int 10 in
        printf "Spout -> %d\n%!" rnd;
        id := !id + 1;
        dispatcher !id rnd
      )
    ;
    Deferred.never ()
  ;;

  let count_func = fun num ->
    printf "Received %d\n%!" num;
    return ()
  ;;

  let spout_desc = Worker_desc.create_simple
    (module Spout)
    ~name:"spout"
    ~init:()
    (`Spout spout_func)

  let count_desc = Worker_desc.create_simple 
    (module Subworker)
    ~name:"count" 
    ~deps:["spout"] 
    (`Worker count_func)
  
end;;


let generate_local_cluster n =
  List.init n ~f:(fun x -> ("localhost", 5000 + x))
;;

let () =
  let builder = Builder.create (generate_local_cluster 2) in
  Builder.add_worker builder Simple_test.spout_desc;
  Builder.add_worker builder Simple_test.count_desc;
  let (network : Network.t) = Builder.build_network builder in
  let module Spawner = (val network.spawner) in
  let command =
    Command.async ~summary:"One spout with one subworker"
      Command.Spec.(empty) 
      (fun () -> Builder.launch builder network ())
  in
  Spawner.run command
;;
