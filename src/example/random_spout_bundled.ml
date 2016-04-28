(** This example shows how to bundle the outputs of multiple workers.
   A sum type is used to tie together the output of spouts with
   different output types. *)

open Core.Std
open Async.Std
open Std

module Bundler_test = struct 

  (** Each worker must provide a module containing the following types:
      
      type input [@@deriving bin_io, sexp]
      type output [@@deriving bin_io, sexp]

      If multiple workers share the same input and output types,
      it is fine to re use the same module. The module will be passed
      as a first-class module to Worker_desc.create_[simple|bundled].

  *)

  module Subworker = struct 
    type input = A of int | B of string [@@deriving bin_io, sexp]
    type output = unit [@@deriving bin_io, sexp]
  end

  module Spout = struct 
    type input = unit [@@deriving bin_io, sexp]
    type output = Subworker.input [@@deriving bin_io, sexp]
  end

  (** A sample spout that produces a value each second.
      Spouts must call the provided dispatcher function
      in order to relay data to workers dependent on them.
      The user is responsible for creating sequence ids.
      Incrementing a counter should be all that's needed,
      here the spout is using randomly ordered ids from 0 to 99,
      for the purpose of testing the ordered sequencer.

      Be sure to return Deferred.never (). *)
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

  (** An example worker that bundles incoming data from two spouts.
      The spouts have distinct output types (int and string), which
      are here tied together using a single sum type. Values are passed
      to the spout's function in the same order as they are listed as
      dependencies in the call to Worker_desc.create, so be sure to
      pattern match in that order if you need to strip away the sum type
      like below. *)
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

  (** This worker has two bundled dependencies, so
      it must provide a function of two arguments,
      tagged with the variant `A2. We have enabled
      ordered sequencing by passing ~sequencer_start. *)
  let count_desc = Worker_desc.create_bundled
    (module Subworker)
    ~name:"count" 
    ~deps:["spout1"; "spout2"] 
    ~sequencer_start:0
    (`A2 count_func)
  
end;;

let generate_local_cluster n =
  List.init n ~f:(fun x -> ("localhost", 5000 + x))
;;

let () =
  let debugger = 
    Debugger.create 
      ~workers:["spout1"] 
      ~f:(printf "N: %s ID: %d MSG: %s\n%!") 
      ~port:7000
  in
  let builder = Builder.create ~debugger (generate_local_cluster 3) in
  Builder.add_worker builder Bundler_test.spout_desc_a;
  Builder.add_worker builder Bundler_test.spout_desc_b;
  Builder.add_worker builder Bundler_test.count_desc;
  let network = Builder.build_network builder in
  let module Spawner = (val network.spawner) in
  let command =
    Command.async ~summary:"One subworker consuming integers from two spouts"
      Command.Spec.(empty) 
      (* After launch, the network of workers is up and 
         spouts are running. *)
      (fun () -> Builder.launch builder network ())
  in
  (* Spawner is the output of Parallel_deprecated.Make. *)
  Spawner.run command
;;
