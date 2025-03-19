(* ===== Import some stuff ===== *)

open Graph.GraphNotation

(* ===== Rules Implementation ===== *)

(* We apply rules 1, 2, 4, 5, 6, 7 to the graph *)

let rules_except_3 wnew g =
  let g, nwnew =
    Graph.IntSet.fold
      begin
        fun i (g, acc) ->
          Graph.IntSet.fold
            begin
              fun j (g, acc) ->
                if Graph.is_white g j then
                  (g <!== (i, j), Graph.IntSet.add j acc)
                else (g, acc)
            end
            (Graph.get_neighbors g i) (g, acc)
      end
      wnew (g, wnew)
  in

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

(* We apply rule 3 to the graph *)

let rec rule_3 s_len s k_max (g, nnwnew) =
  let g, c, s, cand =
    Graph.IntSet.fold
      begin
        fun i (g, c, s, cand) ->
          begin
            match Graph.get_degree g i with
            | 1 when not @@ Graph.IntSet.mem i cand ->
              let u = Graph.IntSet.choose @@ Graph.get_neighbors g i in

              ( g /// i
              , c + 1
              , u :: s
              , Graph.IntSet.union cand @@ Graph.get_neighbors_and_self g u )
            | 2 when not @@ Graph.IntSet.mem i cand ->
              let ni = Graph.get_neighbors g i in
              let u1 = Graph.IntSet.min_elt ni in
              let u2 = Graph.IntSet.max_elt ni in

              if g @? (u1, u2) then
                if Graph.get_degree g u1 = 2 then
                  ( g /// i /// u1
                  , c + 1
                  , u2 :: s
                  , Graph.IntSet.union cand @@ Graph.get_neighbors_and_self g u2
                  )
                else if Graph.get_degree g u2 = 2 then
                  ( g /// i /// u2
                  , c + 1
                  , u1 :: s
                  , Graph.IntSet.union cand @@ Graph.get_neighbors_and_self g u1
                  )
                else (g, c, s, cand)
              else (g, c, s, cand)
            | _ -> (g, c, s, cand)
          end
      end
      nnwnew
      (g, s_len, s, Graph.IntSet.empty)
  in

  if c = s_len then (g, s_len, s)
  else if c > !k_max then (g, c, s)
  else Graph.set_colors g cand White |> rules_except_3 cand |> rule_3 c s k_max

(* We apply all the rules to the graph *)

and reduce_cautious g wnew s_len s k_max =
  Graph.set_colors g wnew White |> rules_except_3 wnew |> rule_3 s_len s k_max

(* ===== Algorithm Implementation ===== *)

(* Parallel stuff *)

exception Stop

let stop = Atomic.make false

let len_min_sol = Atomic.make max_int

let max_completed_k = Atomic.make min_int

(* Auxiliary function of dominating_k *)

let rec dominating_k_aux g s_len s k_min k_max =
  if
    Atomic.get len_min_sol < k_min
    || Atomic.get stop
    || !k_max < Atomic.get max_completed_k
  then raise Stop;

  if
    s_len > !k_max
    || Graph.get_blacknode_count g
       > Graph.max_possible_domination g (!k_max - s_len)
  then None
  else if Graph.get_blacknode_count g = 0 then begin
    if s_len < Atomic.get len_min_sol then Atomic.set len_min_sol s_len;
    k_max := s_len - 1;

    Some s
  end
  else begin
    let rec_f v' =
      (* We reduce the graph *)
      let g', s_len', s' =
        reduce_cautious (g /// v') (Graph.get_neighbors g v') (s_len + 1)
          (v' :: s) k_max
      in

      dominating_k_aux g' s_len' s' k_min k_max
    in

    Graph.IntSet.fold
      begin
        fun v acc ->
          match rec_f v with None -> acc | Some _ as newacc -> newacc
      end
      (Graph.get_neighbors_and_self g @@ Graph.min_deg_blacknode g)
      None
  end

(* Main function of dominating_k *)

let dominating_k g k_min k_max =
  let k_max = ref k_max in
  let g, s_len, s =
    rule_3 0 [] k_max (g, Graph.IntSet.of_list @@ List.init (Graph.len g) Fun.id)
  in

  try
    begin
      let rec loop () =
        match
          dominating_k_aux g s_len s
            (max k_min (Atomic.get max_completed_k))
            k_max
        with
        | None ->
          if Atomic.get max_completed_k < !k_max then
            Atomic.set max_completed_k !k_max;
          k_max := Atomic.get max_completed_k + 1;
          loop ()
        | res -> res
      in
      loop ()
    end
  with Stop -> None

(* Auxiliary function of dominating *)

let dominating_aux g dom_min dom_max =
  (* We reset the global variables *)
  Atomic.set stop false;
  Atomic.set len_min_sol max_int;
  Atomic.set max_completed_k dom_min;

  (* We define the domains for parallelism *)
  let nb_domains = Domain.recommended_domain_count () in
  let c =
    float (dom_max - dom_min) /. float nb_domains |> Float.ceil |> int_of_float
  in

  let args =
    List.init nb_domains (fun i -> (dom_max - ((i + 1) * c), dom_max - (i * c)))
    |> List.filter (fun (k_min, k_max) -> dom_min <= k_min && dom_min <= k_max)
    |> List.sort_uniq compare
  in

  let args =
    let rec loop cpt l =
      if cpt = nb_domains then l
      else
        match l with
        | [] -> []
        | (k_min, k_max) :: l ->
          let k_mid = (k_min + k_max) / 2 in
          (k_min, k_mid) :: (k_mid, k_max) :: loop (cpt + 1) l
    in

    args |> List.rev |> loop (List.length args) |> List.sort_uniq compare
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

  (* We join the results *)
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

  (* We return the result *)
  match res with
  | None -> failwith "No solution"
  | Some res -> res

(* Main function of dominating *)

let dominating g =
  (* We init the args for dominating_aux *)
  let max_deg = Graph.max_deg g in
  let min_size = Graph.min_dom g in
  let max_size = Graph.max_dom g in

  (* We start dominating_aux *)
  dominating_aux (Graph.update_degcount max_deg g) min_size max_size
