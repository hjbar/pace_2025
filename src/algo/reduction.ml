(* NOTE : to obtain the "correct" implementation, all programs here must run in O(n) time. *)

(* Memoisation of reductions *)

(*
let ( (get_reduc : Graph.repr_t -> (Graph.t * int * int list) option)
    , (set_reduc : Graph.repr_t -> Graph.t * int * int list -> unit) ) =
  let mutex = Mutex.create () in
  let ht = Hashtbl.create 16 in

  let get_reduc repr =
    Mutex.lock mutex;
    let opt = Hashtbl.find_opt ht repr in
    Mutex.unlock mutex;
    opt
  in

  let set_reduc repr (graph, k, s) =
    Mutex.lock mutex;
    let () =
      match Hashtbl.find_opt ht repr with
      | None -> Hashtbl.replace ht repr (Graph.copy graph, k, s)
      | Some _ -> ()
    in
    Mutex.unlock mutex
  in

  (get_reduc, set_reduc)
*)

let rule_1 wnew g =
  let open Graph.GraphNotation in
  Graph.IntSet.fold
    begin
      fun i (g, acc) ->
        Graph.IntSet.fold
          (fun j (g, acc) ->
            if Graph.is_white g j then (g <!= (i, j), Graph.IntSet.add j acc)
            else (g, acc) )
          (Graph.get_neighbors g i) (g, acc)
    end
    wnew (g, wnew)

(* Rules 4 to 7 cannot trigger each other or themselves *)

let rules_2_4to7 (g, nwnew) =
  let open Graph.GraphNotation in
  Graph.IntSet.fold
    begin
      fun i (g, acc) ->
        match Graph.get_degree g i with
        | 1 ->
          let u = Graph.IntSet.choose @@ Graph.get_neighbors g i in
          (Graph.remove_edge g u i, Graph.IntSet.add u acc)
        | 2 ->
          let ni = Graph.get_neighbors g i in
          if
            Graph.IntSet.exists (fun u3 -> g @? (u3, Graph.IntSet.max_elt ni))
            @@ Graph.get_neighbors_and_self g
            @@ Graph.IntSet.min_elt ni
          then (g // i, Graph.IntSet.union ni acc)
          else (g, acc)
        | 3 -> begin
          let ni = Graph.get_neighbors g i in
          match Graph.IntSet.elements ni with
          | [ u1; u2; u3 ] ->
            if g @? (u1, u2) then
              if (g @? (u2, u3)) || (g @? (u1, u3)) then
                (g // i, Graph.IntSet.union ni acc)
              else (g, acc)
            else if (g @? (u2, u3)) && (g @? (u1, u3)) then
              (g // i, Graph.IntSet.union ni acc)
            else (g, acc)
          | _ -> failwith "mismatch rule 7"
        end
        | _ -> (g, acc)
    end
    nwnew (g, Graph.IntSet.empty)

let rec rule_3 k s (g, nnwnew) =
  let open Graph.GraphNotation in
  let g, c, s, cand =
    Graph.IntSet.fold
      begin
        fun i (g, c, s, cand) ->
          if Graph.get_degree g i <> 1 || Graph.IntSet.mem i cand then
            (g, c, s, cand)
          else
            let u = Graph.IntSet.choose @@ Graph.get_neighbors g i in
            ( g /// i
            , c + 1
            , u :: s
            , Graph.IntSet.union cand @@ Graph.get_neighbors_and_self g u )
      end
      nnwnew
      (g, 0, s, Graph.IntSet.empty)
  in
  let candsize = Graph.IntSet.cardinal cand in
  if c >= k then (g, 0, s) (* No solution *)
  else if candsize = 0 then (g, k, s)
  else
    Graph.set_colors g cand White
    |> rule_1 cand |> rules_2_4to7
    |> rule_3 (k - c) s

let reduce_cautious (g : Graph.t) (wnew : Graph.IntSet.t) (k : int)
  (s : int list) : Graph.t * int * int list =
  g |> rule_1 wnew |> rules_2_4to7 |> rule_3 k s

(* ===== Paper Implementation ===== *)

(* Parallel stuff *)

exception Stop

let stop = Atomic.make false

let len_min_sol = Atomic.make max_int

let max_deg_glbl = Atomic.make 0

let rec dominating_k_aux (g : Graph.t) (k : int) (s : int list) (k_min : int) :
  int list option =
  let open Graph.GraphNotation in
  (* Parallel stop ? *)
  if Atomic.get len_min_sol < k_min || Atomic.get stop then raise Stop;

  if Graph.get_blacknode_count g = 0 then begin
    let len_s = List.length s in
    if len_s < Atomic.get len_min_sol then Atomic.set len_min_sol len_s;

    Some s
  end
  else if Graph.get_blacknode_count g > k * (Graph.max_deg g + 1) then
    (* else if Graph.get_blacknode_count g > k * (Atomic.get max_deg_glbl + 1) then *)
    None
  else begin
    let rec_f newk v' =
      let nv' = Graph.get_neighbors g v' in
      (* Preprocessing *)
      let g', k', s' =
        reduce_cautious
          (Graph.set_colors (g /// v') nv' White)
          nv' (newk - 1) (v' :: s)
      in
      dominating_k_aux g' k' s' k_min
    in

    let _, res =
      Graph.IntSet.fold
        begin
          fun v (k, acc) ->
            match (rec_f k v, acc) with
            | None, _ -> (k, acc)
            | Some l1, Some l2 when List.compare_lengths l1 l2 >= 0 -> (k, acc)
            | (Some res as acc), _ -> (List.length res - List.length s - 1, acc)
        end
        (Graph.get_neighbors_and_self g @@ Graph.min_deg_blacknode g)
        (k, None)
    in

    res
  end

let dominating_k (g : Graph.t) (k_min : int) (k_max : int) : int list option =
  try dominating_k_aux g k_max [] k_min with Stop -> None

let dominating_paper (g : Graph.t) (min : int) (max : int) : int list =
  Atomic.set stop false;
  Atomic.set len_min_sol max_int;

  let nb_domains = Domain.recommended_domain_count () in
  let c = float (max - min) /. float nb_domains |> Float.ceil |> int_of_float in

  let args =
    List.init nb_domains (fun i -> (max - ((i + 1) * c), max - (i * c)))
    |> List.filter (fun (_, n) -> min <= n)
    |> List.sort_uniq compare
  in

  let doms =
    List.map
      begin
        fun (k_min, k_max) ->
          Domain.spawn @@ fun () ->
          match dominating_k (Graph.copy g) k_min k_max with
          | Some _ as res when not @@ Atomic.get stop ->
            Atomic.set stop true;
            res
          | Some _ as res -> res
          | _ -> None
      end
      args
  in

  let res =
    List.fold_left
      begin
        fun acc d ->
          match (Domain.join d, acc) with
          | None, _ -> acc
          | Some res, None -> Some res
          | Some r1, Some r2 when List.compare_lengths r1 r2 >= 0 -> acc
          | res, _ -> res
      end
      None doms
  in

  match res with None -> failwith "No solution" | Some res -> res

(* == Main Function == *)

let dominating (g : Graph.t) : int list =
  let min_deg = Graph.min_deg g in
  let max_deg = Graph.max_deg g in

  Format.printf "min_deg = %d@\n%!" min_deg;
  Format.printf "max_deg = %d@\n%!" max_deg;

  let min_size = Graph.min_dom g in
  let max_size = Graph.max_dom g in

  Format.printf "min_size = %d@\n%!" min_size;
  Format.printf "max_size = %d@\n%!" max_size;

  Atomic.set max_deg_glbl max_deg;
  dominating_paper g min_size max_size

(* == Appendix: Former Implementations == *)

(* rule 7
begin
  match Graph.get_neighbors_list g i with
  | [ u1; u2; u3 ] ->
    if g @? (u1, u2) then
      if (g @? (u2, u3)) || (g @? (u1, u3)) then g // i else g
    else if (g @? (u2, u3)) && (g @? (u1, u3)) then g // i
    else g
  | _ -> failwith "mismatch rule 7"
end
*)

(* rule 7
let ni = Graph.get_neighbors g i in
let n, _ =
  Graph.IntSet.fold
    (fun u (i, u') -> if g @? (u', u) then (i + 1, u) else (i, u))
    ni
    (0, Graph.IntSet.max_elt ni)
in
if n > 1 then (g // i, Graph.IntSet.union ni acc) else (g, acc)
*)
