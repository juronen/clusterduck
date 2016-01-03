open Core.Std
open Async.Std
open Rpc_parallel_core.Std

module Worker_desc = struct

  type ('input, 'output) t =
    { name    : string
    ; input   : 'input Bin_prot.Type_class.t
    ; output  : 'output Bin_prot.Type_class.t
    ; deps    : string list
    ; func    : 'input -> 'output Deferred.t
    }

  let create ~name ~input ~output ?(deps=[]) ~(f: 'input -> 'output Deferred.t) =
    { name
    ; input
    ; output
    ; deps
    ; func = f
    }

  type any_t = Any : ('input, 'output) t -> any_t

end;;

module Machines = struct

  type t = Host_and_port.t Queue.t

  let create ~machines =
    List.map machines ~f:(fun (host, port) -> Host_and_port.create ~host ~port)
    |> Queue.of_list
  ;;

  let pop_machine t = Queue.dequeue t

end;;

module Network = struct

  type t =
    { roots    : (string * Host_and_port.t) list
    ; workers  : (string * Host_and_port.t) list
    ; spawner  : (module Launch.Spawner_sig)
    } 

end;;

module Builder = struct
 
  type t =
    { descs    : Worker_desc.any_t String.Table.t
    ; machines : Machines.t
    }

  let create ~machines =
    { descs = String.Table.create ()
    ; machines
    }
  ;;

  let add_worker t (desc : ('i, 'o) Worker_desc.t) =
    Hashtbl.replace t.descs ~key:desc.name ~data:(Worker_desc.Any desc)
  ;;

  (* Make workers talk to eachother. E.g. if C depends on A and B, 
   * add one-way RPC dispatches to make 
   * A   B
   *  \ /
   *   C
   *)
  let add_forward_comm ~f ~bin_msg ~(subs : (string * Host_and_port.t) list) = 
    fun () arg ->
    f arg
    >>> fun result ->
    Deferred.List.iter subs ~f:(fun (name, machine) ->
      let host = Host_and_port.host machine in
      let port = Host_and_port.port machine in
      let rpc = Rpc.One_way.create ~name ~version:0 ~bin_msg in
      Rpc.Connection.with_client ~host ~port
        (fun conn ->
           Rpc.One_way.dispatch_exn rpc conn result;
           return ()
        )
      >>| Result.ok_exn
    ) >>> fun () -> ()
  ;;

  let get_execution_levels descs =
    let topo = Topology.create () in
    Hashtbl.iter descs ~f:(fun ~key:name ~data:(Worker_desc.Any desc) ->
      Topology.link topo ~node:name ~deps:desc.deps
    );
    Topology.coffman_graham_levels topo
  ;;

  let implement rpc func = 
    let impl = Rpc.One_way.implement rpc func in
    Rpc.Implementations.create_exn ~implementations:[impl]
      ~on_unknown_rpc:`Close_connection
  ;;

  let build_network t =
    (* I made Worker_shell a functor to make sure that the
     * module (and thus the implementation map) passed to the
     * Parallel.Make functor is unique and not globally accessible.
     * This seemed like good practice, as I think there exists a reasonable
     * (or at least not unthinkable) scenario where some master of all 
     * masters program is used to launch multiple topologies. *)
    let module Worker_shell = Launch.Worker_shell () in
    let {descs; machines} = t in
    (* Map name to Host_and_port.t *)
    let location_map = String.Table.create () in
    let execution_levels = get_execution_levels descs in
    printf "Num levels %d\n%!" (List.length execution_levels);
    let rec process_level ?subtrees levels =
      match levels with
      | []              -> ()
      | cur_level :: tl ->
        let next_subs = String.Table.create () in
        (* For each worker in the current execution level:
             1. Find all the workers that depend on this one
             2. Create a wrapper for this worker's function,
                which sends the result to all the dependants over Rpc
             3. Find workers that this one depends on, add this worker's
                name/host to a table so it's available for step 1
                on the next level.*)
        List.iter cur_level ~f:(fun name ->
          let (Worker_desc.Any desc) = Hashtbl.find_exn descs name in
          let subs = Option.value_map subtrees ~default:[] ~f:(
            fun tbl ->
              Hashtbl.find tbl desc.name |> Option.value ~default:[]
          ) 
          in 
          let func = add_forward_comm ~f:desc.func ~bin_msg:desc.output ~subs in
          let rpc =
            Rpc.One_way.create 
              ~name:desc.name 
              ~version:0 
              ~bin_msg:desc.input 
          in
          let impls = implement rpc func in
          Hashtbl.replace Worker_shell.impls_map ~key:desc.name ~data:impls;
          let box =
            match Machines.pop_machine machines with
            | Some machine -> machine
            | None         -> failwithf "Out of machines for worker %s" desc.name ()
          in
          printf 
            "Assigned machine %s to worker %s"
            (Host_and_port.to_string box) desc.name
          ;    
          Hashtbl.replace
            Worker_shell.ports_map 
            ~key:desc.name
            ~data:(Host_and_port.port box)
          ;
          Hashtbl.replace location_map ~key:desc.name ~data:box;
          (* This seems like a smooth way to accomplish the same as
           * a pattern match on tl here would, and since List.iter can run
           * just fine on an empty list the pattern match just adds
           * syntactic/textual complexity. Is this ok practice, or is the
           * pattern match for [] and hd :: tl preferred because of being
           * quick to parse? *)
          let next_level = List.hd tl |> Option.value ~default:[] in
          List.iter next_level ~f:(fun name ->
            let (Worker_desc.Any desc) = Hashtbl.find_exn descs name in
            let deps = desc.deps in
            if (List.mem deps name) then begin
              let subs = Hashtbl.find next_subs name |> Option.value ~default:[] in
              Hashtbl.replace next_subs ~key:name ~data:((desc.name, box) :: subs) 
            end
          );
        );
        process_level ~subtrees:next_subs tl
    in
    process_level execution_levels;
    (* So now we have a module with a loaded impls map / functional worker_main *)
    let module Spawner = Parallel.Make(Worker_shell) in
    let spawner = (module Spawner : Launch.Spawner_sig) in
    (* We want to separate the roots and the other workers, as
     * the others have to be started before the roots. *)
    let root_names :: tl = List.rev execution_levels in
    let worker_names = List.concat tl in
    let lookup_loc = Hashtbl.find_exn location_map in
    let roots :: workers :: [] = List.map [root_names; worker_names] ~f:(
      fun names -> List.zip_exn names (List.map names ~f:lookup_loc)
    ) 
    in 
    { Network.roots = roots
    ; workers
    ; spawner
    }
  ;;

end;;
