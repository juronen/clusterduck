open Core.Std
open Async.Std
open Rpc_parallel_core.Std

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
  let add_forward_comm 
    ~(desc : ('i, 'o) Worker_desc.t) 
    ~(subs : (string * Host_and_port.t) list) = 
    (* I distinguish between workers with and without subworkers here
     * because if this was to be actually used in a high throughput 
     * environment, the useless call to Deferred.List.iter would
     * probably be undesirable as it actually goes through and
     * creates a new Deferred etc even if the list is empty. *)
    let dispatcher =
      match subs with
      | []   -> `Do_not_dispatch
      | subs ->
        let dispatch_func = fun data ->
          Deferred.List.iter subs ~f:(fun (name, machine) ->
          let host = Host_and_port.host machine in
          let port = Host_and_port.port machine in
          let rpc = Rpc.One_way.create ~name ~version:0 ~bin_msg:desc.output in
          Rpc.Connection.with_client ~host ~port
            (fun conn ->
               return (Rpc.One_way.dispatch rpc conn data)
            )
          >>| fun res ->
          match res with
          | Ok (Ok ())   -> ()
          | Error      e -> 
            printf "Connection failure from %s to %s: %s\n%!" 
              desc.name name (Error.of_exn e |> Error.to_string_hum)
          | Ok (Error e) ->
            printf "Dispatch failure from %s to %s: %s\n%!" 
              desc.name name (Error.to_string_hum e)
          ) >>> fun () -> ()
        in
        `Dispatcher dispatch_func
    in
    match (desc.func, dispatcher) with
    | (Spout  func, `Do_not_dispatch) -> fun () arg -> func arg ignore >>> fun () -> ()
    | (Spout  func, `Dispatcher disp) -> fun () arg -> func arg disp   >>> fun () -> ()
    | (Worker func, `Do_not_dispatch) -> fun () arg -> func arg >>> ignore
    | (Worker func, `Dispatcher disp) -> fun () arg -> func arg >>> disp
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
    let rec process_level ?subtable levels =
      match levels with
      | []                        -> ()
      | cur_level :: upper_levels ->
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
          (* Get the workers that depend on this one, if any *)
          let subworkers = Option.value_map subtable ~default:[] ~f:(
            fun tbl -> Hashtbl.find tbl desc.name |> Option.value ~default:[]
          ) 
          in 
          let func  = add_forward_comm ~desc ~subs:subworkers in
          let box   =
            match Machines.pop_machine machines with
            | Some machine -> machine
            | None         -> failwithf "No machines for worker %s" desc.name ()
          in
          Worker_shell.implement desc func box;
          Hashtbl.replace location_map ~key:desc.name ~data:box; 
          let next_level = List.hd upper_levels |> Option.value ~default:[] in
          List.filter next_level ~f:(List.mem desc.deps) |> List.iter ~f:(
            fun name ->
              let subs = Hashtbl.find next_subs name |> Option.value ~default:[] in
              Hashtbl.replace next_subs ~key:name ~data:((desc.name, box) :: subs)
          );
        ); 
        process_level ~subtable:next_subs upper_levels
    in
    process_level execution_levels;
    (* We want to separate the roots and the other workers, as
     * the others have to be started before the roots, and the roots
     * will require an initial rpc dispatch to start them up. *)
    match List.rev execution_levels with
    | []                         -> failwith "build_network: no workers!" 
    | root_names :: worker_names ->
      let zip_locs names = 
        List.zip_exn names (List.map names ~f:(Hashtbl.find_exn location_map))
      in
      let module Spawner = Parallel.Make(Worker_shell) in
      { Network.roots = zip_locs root_names
      ; workers       = zip_locs (List.concat worker_names)
      ; spawner       = (module Spawner : Launch.Spawner_sig)
      }
  ;;

end;;
