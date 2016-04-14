(** This example shows how to bundle the outputs of multiple workers.
   A sum type is used to tie together the output of spouts with
   different output types. *)

open Core.Std
open Async.Std
open Clusterduck

module Bundler_test = struct 

  module Subworker = struct 
    type input = A of int | B of string [@@deriving bin_io, sexp]
    type output = unit [@@deriving bin_io, sexp]
  end

  module Spout = struct 
    type input = unit [@@deriving bin_io, sexp]
    type output = Subworker.input [@@deriving bin_io, sexp]
  end

  let spout_func = fun () dispatcher ~mapfunc ->
    let data = List.init 100 ~f:Fn.id
      |> List.permute
      |> Queue.of_list
    in
    Clock.every' 
      (Core.Span.of_sec 1.0) 
      (fun () -> 
        let num = Queue.dequeue_exn data in
        printf "Spout -> %d\n%!" num;
        dispatcher num (mapfunc num)
      )
    ;
    Deferred.never ()
  ;;

  let spout_func1 = 
    spout_func ~mapfunc:(fun value -> Subworker.A value)
  ;;

  let spout_func2 =
    spout_func ~mapfunc:(fun value -> Subworker.B (Int.to_string value))
  ;;

  let count_func = fun num1 num2 ->
    let open Subworker in
    let (A a, B b) = (num1, num2) in
    printf "Received %d %s\n%!" a b;
    return ()
  ;;

  let spout_desc_a = Worker_desc.create_simple
    (module Spout)
    ~name:"spout1" 
    ~init:() 
    (`Spout spout_func1)

  let spout_desc_b = Worker_desc.create_simple
    (module Spout)
    ~name:"spout2"
    ~init:()
    (`Spout spout_func2)

  let count_desc = Worker_desc.create_bundled
    (module Subworker)
    ~name:"count" 
    ~deps:["spout1"; "spout2"] 
    ~sequencer_start:0
    (`A2 count_func)
  
end;;

let generate_local_cluster n =
  let hosts = List.init n ~f:(fun _ -> "localhost") in
  let ports = List.init n ~f:(Int.(+) 5000) in
  List.zip_exn hosts ports
;;

let () =
  let builder = Builder.create ~machines:(generate_local_cluster 3) in
  Builder.add_worker builder Bundler_test.spout_desc_a;
  Builder.add_worker builder Bundler_test.spout_desc_b;
  Builder.add_worker builder Bundler_test.count_desc;
  let network = Builder.build_network builder in
  let module Spawner = (val network.spawner) in
  let command =
    Command.async ~summary:"One subworker consuming integers from two spouts"
      Command.Spec.(empty) 
      (fun () -> Builder.launch builder network ())
  in
  Spawner.run command
;;
