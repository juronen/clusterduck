open Core.Std

module Set = Hash_set.Poly

(* (child * parent) *)
type 'a t =
  { edges        : ('a * 'a) Set.t
  ; raise_on_dup : bool
  }

let create ?(raise_on_dup=true) () =
  { edges = Set.create ()
  ; raise_on_dup
  }
;;

let link t ~node ~deps =
  List.iter deps ~f:(fun d ->
    match Set.mem t.edges (node, d) with
    | false -> Set.add t.edges (node, d);
    | true  -> 
      (* Warning to double check your edges *)
      if (t.raise_on_dup) then
        raise (Failure "Duplicate edge!")
  )
;;

let coffman_graham_levels t = 
  let edges = Set.to_list t.edges in
  let rec build_level levels visited =
    (* Find every unvisited node whose parent has been visited *)
    let ready_nodes = List.filter_map edges ~f:(fun (child, parent) ->
      let check = Set.mem visited in
      match (check child, check parent) with
      (* Child is unvisited, parent is visited *)
      | (false, true)     -> Some child
      | (_child, _parent) -> None
    ) |> List.dedup 
    in 
    (* Mark visited after everything is discovered, otherwise
     * it can traverse further down the graph during the filter_map.
     * Because of this we have to dedup above. *)
    List.iter ready_nodes ~f:(fun node -> Set.add visited node);
    match ready_nodes with 
    | []       -> levels 
    | hd :: tl -> build_level (ready_nodes :: levels) visited 
  in
  (* First we find the "root" nodes of the graph,
   * by finding parents which are never the child in an edge.
   * NOTE: this code could be changed to fire off the loop
   * without any initial roots, if I changed the method
   * for entering edges in such a way that a parent must
   * be indicated for each node, while right now they are
   * calculated by the below code. *)
  let children = 
    List.map edges ~f:(fun (child, _parent) -> child)
    |> Set.of_list 
  in
  (* Parents that aren't in the child set are root nodes.
   * We have to dedup in case a root node has multiple children. *)
  let roots = 
    List.filter_map edges ~f:(fun (_ch, parent) ->
      match Set.mem children parent with
      | true  -> None
      | false -> Some parent 
    )
    |> List.dedup
  in
  build_level [roots] (Set.of_list roots) 
;;
(*
let () =
  let graph = create () in
  let lnk = link graph in
  lnk ~deps:["2"] ~node:"6";
  lnk ~deps:["1"] ~node:"5";
  lnk ~deps:["1"] ~node:"4";
  lnk ~deps:["1"] ~node:"3";
  lnk ~deps:["3"; "4"; "5"] ~node:"7";
  lnk ~deps:["6"] ~node:"8";
  lnk ~deps:["7"; "8"] ~node:"9";
  coffman_graham_levels graph
  |> List.iter ~f:(fun ls -> List.iter ls ~f:(printf "%s %!"); printf "\n%!")
*)
