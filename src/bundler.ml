open Core.Std
open Async.Std

module Dyn_application = struct

  type ('a, 'b) func_type = 
    [ `A1 of 'a -> 'b
    | `A2 of 'a -> 'a -> 'b
    | `A3 of 'a -> 'a -> 'a -> 'b
    | `A4 of 'a -> 'a -> 'a -> 'a -> 'b 
    | `A5 of 'a -> 'a -> 'a -> 'a -> 'a -> 'b
    ] 

  let apply f args =
    match (f, args) with
    | (`A1 f, [a])             -> f a
    | (`A2 f, [a; b])          -> f a b
    | (`A3 f, [a; b; c])       -> f a b c
    | (`A4 f, [a; b; c; d])    -> f a b c d
    | (`A5 f, [a; b; c; d; e]) -> f a b c d e
    | (_, _)                   ->
      failwith "Number of args (0 < c < 6) does not match function specifier"
  ;;
           
end;;

module type Fill_type = sig

  type t 

end;;

module Fill_result = struct

  type 'a t = [`Active | `New of 'a list Deferred.t]

end

module Make_bundle (D : Fill_type) = struct

  (** The reason we need to store the keys even after the creation of
      the table is to make sure [wait_complete] returns the contents
      in the same order as the list that was passed in to [create] is.

      Relying on the table's ordering of the keys has resulted in bad times. *)

  type t = 
    { keys : string list
    ; tbl : D.t Ivar.t String.Table.t
    }

  let create keys =
    let tbl = 
      List.init (List.length keys) ~f:(fun _ -> Ivar.create ())
      |> List.zip_exn keys
      |> String.Table.of_alist_exn 
    in
    { keys; tbl }
  ;;

  let fill t key value = 
    let ivar = Hashtbl.find_exn t.tbl key in
    Ivar.fill ivar value
  ;;

  let wait_complete t =
    List.map t.keys ~f:(fun key ->
      Ivar.read (Hashtbl.find_exn t.tbl key)
    )
    |> Deferred.all
  ;;

end;;

module Make (D : Fill_type) = struct

  module Bundle = Make_bundle(D)

  type t =
    { keys : string list
    ; bundles : (int, Bundle.t) Hashtbl.t
    }

  let create keys =
    { keys
    ; bundles = Hashtbl.create ~hashable:Int.hashable ()
    }
  ;;

  let update t seq_id key value =
    match Hashtbl.find t.bundles seq_id with
    | Some bundle -> 
      Bundle.fill bundle key value; 
      `Active
    | None        -> 
      let bundle = Bundle.create t.keys in
      Hashtbl.set t.bundles ~key:seq_id ~data:bundle;
      Bundle.fill bundle key value;
      let bundle =
        Bundle.wait_complete bundle
        >>| fun args_list ->
        Hashtbl.remove t.bundles seq_id;
        args_list
      in
      `New bundle
  ;;

end;;
