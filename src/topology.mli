open Core.Std

(** This module provides the tools for resolving the computation order of 
    interdependent tasks in a graph that represents a distributed computation.

    Usage:

      Say we have the somewhat complicated topology below, where the flow is
      "down", i.e. T6 depends on T2.

          T1                 T2
        / | \                |
       /  |  \               |
      T3  T4  T5             T6
       \  |  /               |
        \ | /                |
          T7                 T8
           \                /
            \_____ T9 _____/

      
      In this case, we'd have the Coffman-Graham levels:
        T1, T2
        T3, T4, T5, T6
        T7, T8
        T9,

      which can be computed as such:

        create ()
        |> link ~deps:["T2"] ~name:"T6"
        |> link ~deps:["T1"] ~name:"T5"
        |> link ~deps:["T1"] ~name:"T4"
        |> link ~deps:["T1"] ~name:"T3"
        |> link ~deps:["T3"; "T4"; "T5"] ~name:"T7"
        |> link ~deps:["T6"] ~name:"T8"
        |> link ~deps:["T7"; "T8"] ~name:"T9"
        |> coffman_graham_levels
        |> List.iter ~f:(fun ls -> List.iter ls ~f:(printf "%s %!"); printf "\n%!")

    The above code collects every edge in the graph, and then generates the levels.

*)

(** [t] contains the edges that form a graph, the raise_on_dup flag from [create]. *)
type 'a t

(** [?raise_on_dup] is a switch to control whether an exception is raised
    when the same edge is entered multiple times. If this happens it might be
    time to make sure the graph formed by the edges looks how you think it does.
*)
val create : ?raise_on_dup:bool -> unit -> 'a t

(** [link] is used to add a new edge to the graph. [node] is the id of the
    node to add, while [deps] is the list of dependencies that node has, or its
    parent nodes in other words. *)
val link : 'a t -> node:'a -> deps:'a list -> unit

(** [coffman_graham_levels] returns the nodes of the graph organized into
    levels such that each task in level N+1 is ready to compute
    dependency-wise, after each task in level N has completed. *)
val coffman_graham_levels : 'a t -> 'a list list
