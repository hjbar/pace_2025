let rec all_subset = function
  | [] -> [ ([], 0) ]
  | x :: l ->
    let subsets = all_subset l in
    let new_subsets =
      List.map
        (fun (subset, subset_len) -> (x :: subset, subset_len + 1))
        subsets
    in
    subsets @ new_subsets

let is_dominating (g : Graph.t) (n : int) (s : int list) =
  let c = List.fold_left (fun acc v -> (v :: Graph.neighbors g v) @ acc) [] s in
  let c = List.sort_uniq compare c in
  List.length c = n

let dominating (g : Graph.t) : int list =
  let n = Graph.len g in
  let v = List.init n (fun i -> i) in

  let res, _ =
    List.fold_left
      begin
        fun (best, best_len) (subset, subset_len) ->
          if is_dominating g n subset && subset_len < best_len then
            (subset, subset_len)
          else (best, best_len)
      end
      (v, n) (all_subset v)
  in

  res
