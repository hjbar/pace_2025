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

(* ===== Paper Implementation ===== *)

(*

(* Computes (b /\ nv, w /\ nv) *)
let intersect_bw len b w nv : int list * int list =
  let dummy = Array.init len (Fun.const false) in
  List.iter (fun i -> dummy.(i) = true) nv;
  let b' = List.filter (fun x -> dummy.(i)) b in
  let w' = List.filter (fun x -> dummy.(i)) w in
  (b', w')

(* Computes b \ nv *)
let exclude len b nv : int list =
  let dummy = Array.init len (Fun.const true) in
  List.iter (fun i -> dummy.(i) = false) nv;
  List.filter (fun x -> dummy.(i)) b

let union len w nv : int list = exclude len w nv @ nv

let exclude_edges (g : Graph.t) (v : int) : Graph.t =
  Array.mapi
    begin
      fun i row ->
        if i = v then Array.make (Graph.len g) 0
        else (
          Array.set row v 0;
          row )
    end
    g

let pickminv (b : int list) (g : Graph.t) : int =
  List.fold_left
    begin
      fun acc v ->
        let nb = Graph.nb_neighbors g v in
        if nb < acc then nb else acc
    end
    (1 + Graph.len g)
    b

let rec dominating_k_aux (b : int list) (w : int list) (g : Graph.t)
  (gi : int array) (wnew : int list) (k : int) (s : int list) : int list option =
  (* Preprocessing *)
  let b, w, g, gi, k, s = Reduction.reduce_cautious b w g gi wnew k s in
  
  if k = 0 then if b = [] && w = [] then Some s else None
  else
    let len = Graph.len g in
    let v = pickminv b g in
    let b', w' = intersect_bw len b w (v :: Graph.neighbors g v) in

    let f_b' =
      begin
        fun v' ->
          let nv = Graph.neighbors g v' in
          let brec = exclude len b (v' :: nv) in
          let wrec = union len w nv in
          let grec = exclude_edges g v' in

          (* girec needs to be updated from gi rather than initiated. *)
          (* It also can't be a list for time complexity reasons *)
          let girec = Array.init (Graph.len g) (Graph.nb_neighbors grec) in

          dominating_k_aux brec wrec grec girec nv (k - 1) (v' :: s)
      end
    in
    let s' = List.find_map f_b' b' in
    if s' = None then
      let f_w' =
        begin
          fun v' ->
            let nv = Graph.neighbors g v' in
            let brec = exclude len b nv in
            let wrec = exclude len (union len w nv) [ v' ] in
            let grec = exclude_edges g v' in

            (* Same problem as above; temporary solution *)
            let girec = Array.init (Graph.len g) (Graph.nb_neighbors grec) in

            dominating_k_aux brec wrec grec girec nv (k - 1) (v' :: s)
        end
      in
      List.find_map f_w' w'
    else s'

let dominating_k (g : Graph.t) (k : int) : int list option =
  (* `b` is the set of black vertices (non-dominated) *)
  (* `w` is the set of white vertices (dominated) *)
  (* `s` is a candidate partial solution *)
  let b = List.init (Graph.len g) (fun x -> x) in
  let w = [] in
  let s = [] in
  let gi = Array.init (Graph.len g) (Graph.nb_neighbors g) in
  dominating_k_aux b w g gi [] k s


*)
