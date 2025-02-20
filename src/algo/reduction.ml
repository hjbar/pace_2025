(* NOTE : to obtain the "correct" implementation, all programs here must run in O(n) time. *)

(* Rules 1 and 2 can be applied before all the other rules *)
(* Rules >1 cannot trigger Rule 1, and Rules >2 cannot trigger Rule 2 *)

let rule_1 wnew g =
  Graph.map_like (Graph.on_white @@ Graph.remove_neighbors wnew) g

let rule_2 g = g

(* Below implementation is wrong, a simple pass isn't enough *)
(*
let rule_2 g =
  let delete_hanging g i =
    if Graph.get_degree g i = 1 then Graph.ignore_node g i else g
  in
  Graph.map_like (Graph.on_white delete_hanging) g
*)

(* Rule 3 is optional *)
(* Rule 3 can trigger Rules 4, 5', 6', 7 and be triggered by them.*)
(* If implemented, maybe run it twice, once before rules 4-7 and once after *)

let rule_3 g k s = (g, k, s)

(* I believe rules 4 to 7 cannot trigger each other or themselves *)

let rule_4 g = g

let rule_5' g = g

let rule_6' g = g

let rule_7 g = g

let reduce_cautious (g : Graph.t) (wnew : int list) (k : int) (s : int list) :
  Graph.t * int * int list =
  let g = g |> rule_1 wnew |> rule_2 in

  let g, k, s = rule_3 g k s in

  let g = g |> rule_4 |> rule_5' |> rule_6' |> rule_7 in

  rule_3 g k s

(* ===== Paper Implementation ===== *)

let rec_graph g v' : Graph.t =
  let nv = Graph.get_neighbors g v' in
  let g' = Graph.set_colors_from_set g nv White in
  Graph.ignore_node g' v'

(* Black vertices are non-dominated *)
(* White vertices are dominated *)
let rec dominating_k_aux (g : Graph.t) (wnew : int list) (k : int) (s : int list)
  : int list option =
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

    let s' = List.find_map rec_f b' in
    if s' = None then List.find_map rec_f w' else s'

let dominating_k (g : Graph.t) (k : int) : int list option =
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
