(* test choice constant here *)

type algo =
  | Naive
  | Paper

let algo_choice = Naive

(* Naive algorithm *)

let iter_subsets f l =
  let rec loop set prefix n =
    match set with
    | [] -> f prefix n
    | x :: set ->
      loop set (x :: prefix) (n + 1);
      loop set prefix n
  in
  loop l [] 0

let is_dominating (g : Graph.t) (n : int) (s : int list) =
  let c =
    List.fold_left (fun acc v -> (v :: Graph.get_neighbors_list g v) @ acc) [] s
  in
  let c = List.sort_uniq compare c in
  List.length c = n

let dominating_naive (g : Graph.t) (min_size : int) (max_size : int) : int list
    =
  let n = Graph.len g in
  let v = List.init n (fun i -> i) in

  let best = ref v in
  let best_len = ref n in

  iter_subsets
    begin
      fun subset subset_len ->
        if
          min_size <= subset_len && subset_len <= max_size
          && subset_len < !best_len && is_dominating g n subset
        then begin
          best_len := subset_len;
          best := subset
        end
    end
    v;

  !best

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
  let g, k, s = Reduction.reduce_cautious g wnew k s in

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

  begin
    match algo_choice with
    | Naive -> dominating_naive
    | Paper -> dominating_paper
  end
    g min_size max_size
