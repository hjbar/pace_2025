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
    |> List.sort_uniq compare
  in
  List.length c = n

let dominating_aux (g : Graph.t) (min_size : int) (max_size : int) : int list =
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

let dominating (g : Graph.t) : int list =
  let min_size = Graph.min_dom g in
  let max_size = Graph.max_dom g in
  dominating_aux g min_size max_size
