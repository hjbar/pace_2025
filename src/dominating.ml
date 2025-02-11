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
  let n = Graph.len g in
  let v = List.init n (fun i -> i) in

  let best = ref v in
  let best_len = ref n in

  iter_subsets
    begin
      fun subset subset_len ->
        if is_dominating g n subset && subset_len < !best_len then begin
          best := subset;
          best_len := subset_len
        end
    end
    v;

  !best
