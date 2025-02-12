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
  let c = List.fold_left (fun acc v -> (v :: Graph.neighbors g v) @ acc) [] s in
  let c = List.sort_uniq compare c in
  List.length c = n

let dominating (g : Graph.t) : int list =
  let min_deg = Graph.min_deg g in
  let max_deg = Graph.max_deg g in

  Format.printf "min_deg = %d@\n%!" min_deg;
  Format.printf "max_deg = %d@\n%!" max_deg;

  let min_size = Graph.min_dom g in
  let max_size = Graph.max_dom g in

  Format.printf "min_size = %d@\n%!" min_size;
  Format.printf "max_size = %d@\n%!" max_size;

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
