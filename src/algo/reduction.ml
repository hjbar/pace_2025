(* NOTE : to obtain the "correct" implementation, all programs here must run in O(n) time. *)

(* Rules 1 and 2 can be applied before all the other rules *)
(* Rules >1 cannot trigger Rule 1, and Rules >2 cannot trigger Rule 2 *)

let rule_1 wnew g =
  Graph.map_like (Graph.on_white @@ Graph.remove_neighbors wnew) g

(* A simple pass is sufficient since we can assume rule 1 has been applied *)
(* This rule might also be able to be applied simultaneously with rules 4 to 7 *)
let rule_2 g =
  Graph.map_like (Graph.on_deg 1 @@ Graph.on_white Graph.ignore_node) g

(* Rule 3 is optional *)
(* Rule 3 can trigger Rules 4, 5', 6', 7 and be triggered by them.*)
(* If implemented, maybe run it twice, once before rules 4-7 and once after *)

let rule_3 g k s = (g, k, s)

(* I believe rules 4 to 7 cannot trigger each other or themselves *)

(* This is a messy workaround. *)
(* It also only works because of some internal knowledge about Graph: *)
(* We know that get_neighbors_list returns the neighbors in sorted ascending order. *)
let rule_5_6_helper : (int * int, int) Hashtbl.t = Hashtbl.create 100

(* There's a slight hack here that loses in performance, but that has no easy solution.
 * It may perform some redundant checks performed by rule_5_6_row_part2.
 * The alternative is to have g' be a directed graph, or some other comnpromise. *)
let rule_5_6_row_part1 g' g i =
  let open Graph.GraphNotation in
  match Graph.get_neighbors_list g i with
  | [ u1; u2 ] when Graph.is_black g u1 && Graph.is_black g u2 ->
    let degu1' = Graph.get_degree g' u1 in
    let degu2' = Graph.get_degree g' u2 in
    begin
      match (degu1', degu2') with
      | 0, _ ->
        (* u1 is of degree < 4 *)
        Hashtbl.add rule_5_6_helper (u1, u2) i;
        let nu1 = Graph.get_neighbors_list g u1 in
        if List.exists (fun x -> g @? (x, u2)) nu1 then g // i else g
      | _, 0 ->
        (* u2 is of degree < 4 *)
        Hashtbl.add rule_5_6_helper (u1, u2) i;
        let nu2 = Graph.get_neighbors_list g u2 in
        if List.exists (fun x -> g @? (x, u1)) nu2 then g // i else g
      | d1, _ when d1 < 8 ->
        Hashtbl.add rule_5_6_helper (u1, u2) i;
        let nu1 = Graph.get_neighbors_list g' u1 in
        if List.exists (fun x -> g @? (x, u2)) nu1 then g // i else g
      | _, d2 when d2 < 8 ->
        Hashtbl.add rule_5_6_helper (u1, u2) i;
        let nu2 = Graph.get_neighbors_list g' u2 in
        if List.exists (fun x -> g @? (x, u1)) nu2 then g // i else g
      | _ -> g
    end
  | _ -> failwith "mismatch rule 5/6 pt1"

let rule_5_6_row_part2 g i =
  let open Graph.GraphNotation in
  let rem (u1, u2) g =
    begin
      match Hashtbl.find_opt rule_5_6_helper (u1, u2) with
      | Some w -> g // w
      | None -> g
    end
  in
  match Graph.get_degree g i with
  | 2 -> begin
    match Graph.get_neighbors_list g i with
    | [ u1; u2 ] when Graph.is_black g u1 && Graph.is_black g u2 ->
      g |> rem (u1, u2)
    | [ _; _ ] -> g
    | _ -> failwith "mismatch rule 5/6 pt2"
  end
  | 3 -> begin
    match Graph.get_neighbors_list g i with
    | [ u1; u2; u3 ]
      when Graph.is_black g u1 && Graph.is_black g u2 && Graph.is_black g u3 ->
      g |> rem (u1, u2) |> rem (u2, u3) |> rem (u1, u3)
    | [ _; _; _ ] -> g
    | _ -> failwith "mismatch rule 5/6 pt2"
  end
  | _ -> g

let rule_5_6' g =
  let g' = Graph.g_over_4 g in
  g
  |> Graph.map_like @@ Graph.on_deg 2 @@ Graph.on_white @@ rule_5_6_row_part1 g'
  |> Graph.map_like rule_5_6_row_part2

(* Technically, checking for Graph.is_black is redundant,
 * since i is white and rule 1 has been applied. *)
let rule_4_7_row g i =
  let open Graph.GraphNotation in
  match Graph.get_degree g i with
  | 2 ->
    (* rule 4 *)
    begin
      match Graph.get_neighbors_list g i with
      | [ u1; u2 ] when Graph.is_black g u1 && Graph.is_black g u2 ->
        if g @? (u1, u2) then g // i else g
      | _ -> failwith "mismatch rule 4"
    end
  | 3 ->
    (* rule 7 *)
    begin
      match Graph.get_neighbors_list g i with
      | [ u1; u2; u3 ]
        when Graph.is_black g u1 && Graph.is_black g u2 && Graph.is_black g u3
        ->
        if g @? (u1, u2) then
          if (g @? (u2, u3)) || (g @? (u1, u3)) then g // i else g
        else if (g @? (u2, u3)) && (g @? (u1, u3)) then g // i
        else g
      | _ -> failwith "mismatch rule 7"
    end
  | _ -> g

let rule_4_7 g = Graph.map_like (Graph.on_white rule_4_7_row) g

let reduce_cautious (g : Graph.t) (wnew : int list) (k : int) (s : int list) :
  Graph.t * int * int list =
  let g = g |> rule_1 wnew |> rule_2 in

  let g, k, s = rule_3 g k s in

  let g = g |> rule_4_7 |> rule_5_6' in

  Hashtbl.clear rule_5_6_helper;
  rule_3 g k s

(* ===== Paper Implementation ===== *)

let rec_graph g v' : Graph.t =
  let nv = Graph.get_neighbors_list g v' in
  let g' = Graph.set_colors g nv White in
  Graph.ignore_node g' v'

(* Parallel stuff *)

(*
exception Stop

let stop = Atomic.make false
*)

(* Black vertices are non-dominated *)
(* White vertices are dominated *)
let rec dominating_k_aux (g : Graph.t) (wnew : int list) (k : int) (s : int list)
  : int list option =
  (* Parallel stop ? *)
  (* if Atomic.get stop then raise Stop; *)

  (* Preprocessing *)
  let g, k, s = reduce_cautious g wnew k s in

  let nb_black = Graph.get_blacknode_count g in
  (* let nb_white = Graph.get_whitenode_count g in *)

  (*
   * Note: Below is what's written in the paper, but I'm not convinced of it.
   * If B is empty and W is not, isn't a dominating set found?
   *  if k = 0 then if nb_black = 0 && nb_white = 0 then Some s else None
   *)
  if nb_black = 0 then Some s
  else if k = 0 then None
  else
    let v = Graph.min_deg_blacknode g in
    let b', w' =
      Graph.get_bw_inter_with_set g (Graph.get_neighbors_and_self g v)
    in

    let rec_f v' =
      dominating_k_aux (rec_graph g v')
        (Graph.get_neighbors_list g v')
        (k - 1) (v' :: s)
    in

    (* let s' = List.find_map rec_f b' in *)
    let s' =
      List.fold_left
        begin
          fun acc e ->
            match (rec_f e, acc) with
            | None, _ -> acc
            | Some l1, Some l2 when List.compare_lengths l1 l2 >= 0 -> acc
            | res, _ -> res
        end
        None b'
    in
    (* if s' = None then List.find_map rec_f w' else s' *)
    if s' = None then
      List.fold_left
        begin
          fun acc e ->
            match (rec_f e, acc) with
            | None, _ -> acc
            | Some l1, Some l2 when List.compare_lengths l1 l2 >= 0 -> acc
            | res, _ -> res
        end
        None w'
    else s'

let dominating_k (g : Graph.t) (k : int) : int list option =
  (* try dominating_k_aux g [] k [] with Stop -> None *)
  dominating_k_aux g [] k []

(* Note : huge inefficiency here *)
let rec dominating_binsearch (g : Graph.t) (min : int) (max : int)
  (smax : int list) : int list =
  match max - min with
  | 0 -> failwith "Binary search failed to find a dominating set"
  | 1 -> smax
  | _ ->
    let k = (max + min) / 2 in
    begin
      match dominating_k g k with
      | Some s -> dominating_binsearch g min k s
      | None -> dominating_binsearch g k max smax
    end

let dominating_paper (g : Graph.t) (min : int) (max : int) : int list =
  match dominating_k g min with
  | Some s -> s (* Minimum dominating set size is viable *)
  | None -> begin
    match dominating_k g max with
    | None -> failwith "Maximum dominating set size is not viable"
    | Some s -> dominating_binsearch g min max s
  end

(*
let dominating_paper (g : Graph.t) (min : int) (max : int) : int list =
  Atomic.set stop false;

  let nb_domains = Domain.recommended_domain_count () in
  let c = float (max - min) /. float nb_domains |> Float.ceil |> int_of_float in

  let args =
    List.init nb_domains (fun i -> max - (i * c))
    |> List.filter (fun n -> min <= n)
    |> List.sort_uniq compare
  in

  List.iter (fun i -> Format.printf "%d %!" i) args;
  Format.printf "@\n%!";

  let doms =
    List.map
      begin
        fun n ->
          Domain.spawn @@ fun () ->
          match dominating_k (Graph.copy g) n with
          | Some _ as res when not @@ Atomic.get stop ->
            Atomic.set stop true;
            Format.printf "Res of f(%d) = %d@\n%!" n
              (List.length @@ Option.get res);
            res
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
*)

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

  dominating_paper g min_size max_size
