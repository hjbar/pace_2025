(* Undirected unweighted (simple, in practice) graph *)

module IntSet = Set.Make (Int)

type color =
  | Black
  | White

type t =
  { deg : int Parray.t
  ; adj : IntSet.t Parray.t
  ; col : color Parray.t
  ; nb_b : int
  }

(*
type repr_t =
  int list * IntSet.t list * color list * int * IntSet.t * int * int list
*)

(* Utility Functions *)

let init_empty (n : int) : t =
  { deg = Parray.init n (Fun.const 0)
  ; adj = Parray.init n (Fun.const IntSet.empty)
  ; col = Parray.init n (Fun.const Black)
  ; nb_b = n
  }

let define_t deg adj col nb_b = { deg; adj; col; nb_b }

let copy g =
  let copy_parray a = Parray.init (Parray.length a) (fun i -> Parray.get a i) in
  define_t (copy_parray g.deg) (copy_parray g.adj) (copy_parray g.col) g.nb_b

(*
let get_repr g wnew k s : repr_t =
  ( Parray.to_list g.deg
  , Parray.to_list g.adj
  , Parray.to_list g.col
  , g.nb_b
  , wnew
  , k
  , s )
*)

(* == Get functions == *)

let len g = Parray.length g.deg

let get_neighbors g i = Parray.get g.adj i

let get_neighbors_and_self g i = IntSet.add i @@ Parray.get g.adj i

let get_degree g i = Parray.get g.deg i

let get_color g i = Parray.get g.col i

let is_white g i = get_color g i = White

let is_black g i = get_color g i = Black

let is_singleton g i = Parray.get g.deg i = 0

let is_edge g i j =
  if is_singleton g i || is_singleton g j then false
  else
    let row = Parray.get g.adj i in
    IntSet.find_opt j row |> Option.is_some

(* == Modifications == *)

let add_edge (g : t) i j : t =
  if is_edge g i j then g
  else
    let degi = Parray.get g.deg i in
    let degj = Parray.get g.deg j in
    let newrowi = IntSet.add j @@ Parray.get g.adj i in
    let newrowj = IntSet.add i @@ Parray.get g.adj j in

    let newdeg = Parray.set (Parray.set g.deg i (degi + 1)) j (degj + 1) in
    let newadj = Parray.set (Parray.set g.adj i newrowi) j newrowj in
    define_t newdeg newadj g.col g.nb_b

let remove_edge (g : t) i j : t =
  if not @@ is_edge g i j then g
  else
    let degi = Parray.get g.deg i in
    let degj = Parray.get g.deg j in
    let newrowi = IntSet.remove j @@ Parray.get g.adj i in
    let newrowj = IntSet.remove i @@ Parray.get g.adj j in

    let newdeg = Parray.set (Parray.set g.deg i (degi - 1)) j (degj - 1) in
    let newadj = Parray.set (Parray.set g.adj i newrowi) j newrowj in
    define_t newdeg newadj g.col g.nb_b

(* ----- *)

let add_edges (g : t) (es : (int * int) list) : t =
  List.fold_left (fun g (i, j) -> add_edge g i j) g es

let remove_edges (g : t) (es : (int * int) list) : t =
  List.fold_left (fun g (i, j) -> remove_edge g i j) g es

let set_color g i c : t =
  if Parray.get g.col i = c then g
  else
    let newcol = Parray.set g.col i c in
    define_t g.deg g.adj newcol (if c = Black then g.nb_b + 1 else g.nb_b - 1)

let set_colors g vs c = IntSet.fold (fun i g -> set_color g i c) vs g

let get_blacknode_count g = g.nb_b

(* == Other Utility == *)

(* Not quite a map, since a graph is not iterable *)
(* (t -> int -> t) -> t -> t *)
let map_like f g =
  let len_ = len g in
  let rec iter i g =
    begin
      match i with i' when i' = len_ -> g | _ -> iter (i + 1) @@ f g i
    end
  in
  iter 0 g

(* (acc -> t -> int -> acc) -> acc -> t -> acc *)
let fold_left_like f init g =
  let len_ = len g in
  let rec fold i g acc =
    begin
      match i with i' when i' = len_ -> acc | _ -> fold (i + 1) g @@ f acc g i
    end
  in
  fold 0 g init

(* (t -> int -> t) -> (t -> int -> t) *)
let on_white f g i = if get_color g i = White then f g i else g

let on_black f g i = if get_color g i = Black then f g i else g

let on_deg d f g i = if get_degree g i = d then f g i else g

let on_deg_nz f g i = if get_degree g i <> 0 then f g i else g

(* == Other Functions specifically useful for the Algorithm == *)

(* == Naive Algorithm == *)
let get_neighbors_list g i = IntSet.elements @@ get_neighbors g i

(* == Reduction Algorithm == *)

(* Used multiple times when a node is "removed" *)
let strip_white_node g i =
  let g =
    IntSet.fold
      begin
        fun n g ->
          define_t
            (Parray.set g.deg n (Parray.get g.deg n - 1))
            (Parray.set g.adj n @@ IntSet.remove i @@ Parray.get g.adj n)
            g.col g.nb_b
      end
      (get_neighbors g i) g
  in

  let newdeg = Parray.set g.deg i 0 in
  let newadj = Parray.set g.adj i IntSet.empty in

  define_t newdeg newadj g.col g.nb_b

let min_deg_blacknode g =
  let rec min_aux g i (node, deg) =
    if i = len g then node
    else if is_white g i then min_aux g (i + 1) (node, deg)
    else
      let d = get_degree g i in
      if d < 3 then i
      else min_aux g (i + 1) (if deg > d then (i, d) else (node, deg))
  in
  min_aux g 0 (0, len g)

(* == Function for min_deg == *)

let min_deg g =
  Parray.fold_left (fun acc i -> if acc > i then i else acc) (len g) g.deg

(* == Function for max_deg == *)

let max_deg g =
  Parray.fold_left (fun acc i -> if acc < i then i else acc) 0 g.deg

(* == Function for excentricity (radius & diameter) == *)

let floyd_warshall g =
  let len = len g in

  let dist = Array.make_matrix len len max_int in
  for i = 0 to len - 1 do
    dist.(i).(i) <- 0
  done;

  for i = 0 to len - 1 do
    IntSet.iter
      begin
        fun j ->
          dist.(i).(j) <- 1;
          dist.(j).(i) <- 1
      end
      (get_neighbors g i)
  done;

  for k = 0 to len - 1 do
    for i = 0 to len - 1 do
      for j = 0 to len - 1 do
        let ik = dist.(i).(k) in
        let kj = dist.(k).(j) in
        let ij = dist.(i).(j) in

        if ik <> max_int && kj <> max_int then dist.(i).(j) <- min ij (ik + kj)
      done
    done
  done;

  dist

let excentricity g =
  let len = len g in
  let dist = floyd_warshall g in

  let exc = Array.init len (fun i -> Array.fold_left max min_int dist.(i)) in

  let r = Array.fold_left min max_int exc in
  let d = Array.fold_left max min_int exc in

  (r, d)

(* == Functions for min_dom == *)

let min_dom_b1 ~len ~max_deg =
  float len /. float (max_deg + 1) |> Float.floor |> int_of_float

let min_dom_b2 ~len ~diameter =
  if len <= 1 then min_int
  else float (diameter + 1) /. 3. |> Float.floor |> int_of_float

let min_dom_b3 ~len ~radius =
  if len <= 1 then min_int
  else float (2 * radius) /. 3. |> Float.floor |> int_of_float

let min_dom g =
  let len = len g in
  let max_deg = max_deg g in
  let radius, diameter = excentricity g in

  let b1 = min_dom_b1 ~len ~max_deg in
  let b2 = min_dom_b2 ~len ~diameter in
  let b3 = min_dom_b3 ~len ~radius in

  List.fold_left max min_int [ b1; b2; b3 ]

(* == Functions for max_dom == *)

let max_dom_b1 ~len ~max_deg = len - max_deg

let max_dom_b2 ~len = float len /. 2. |> Float.ceil |> int_of_float

let max_dom_b3 ~len ~min_deg =
  if min_deg < 2 then max_int
  else float (len + 2) /. 3. |> Float.ceil |> int_of_float

let max_dom_b4 ~len ~min_deg =
  if min_deg < 2 then max_int
  else float (2 * len) /. 5. |> Float.ceil |> int_of_float

let max_dom_b5 ~len ~min_deg =
  if min_deg < 3 then max_int
  else float (3 * len) /. 8. |> Float.ceil |> int_of_float

let max_dom_b6 ~len ~min_deg =
  if min_deg < 4 then max_int
  else float (4 * len) /. 11. |> Float.ceil |> int_of_float

let max_dom_b7 ~len ~min_deg =
  if min_deg < 5 then max_int else float len /. 3. |> Float.ceil |> int_of_float

let max_dom_b8 ~len ~min_deg =
  if min_deg < 6 then max_int
  else float (127 * len) /. 418. |> Float.ceil |> int_of_float

let max_dom_b9 ~len ~min_deg =
  if min_deg < 1 then max_int
  else begin
    let d = min_deg + 1 in
    let cpt = ref 0. in

    for j = 1 to d do
      cpt := !cpt +. (1. /. float j)
    done;

    let c1 = float len /. float d in
    let c2 = !cpt in
    c1 *. c2 |> Float.ceil |> int_of_float
  end

let max_dom g : int =
  let len = len g in
  let min_deg = min_deg g in
  let max_deg = max_deg g in

  let b1 = max_dom_b1 ~len ~max_deg in
  let b2 = max_dom_b2 ~len in
  let b3 = max_dom_b3 ~len ~min_deg in
  let b4 = max_dom_b4 ~len ~min_deg in
  let b5 = max_dom_b5 ~len ~min_deg in
  let b6 = max_dom_b6 ~len ~min_deg in
  let b7 = max_dom_b7 ~len ~min_deg in
  let b8 = max_dom_b8 ~len ~min_deg in
  let b9 = max_dom_b9 ~len ~min_deg in

  List.fold_left min max_int [ b1; b2; b3; b4; b5; b6; b7; b8; b9 ]

(* == Functions for debugging == *)

let total_edges g = Parray.fold_left (fun i acc -> acc + i) 0 g.deg

(* Notation for ease of use *)

module GraphNotation = struct
  let ( @? ) g (i, j) = is_edge g i j

  let ( <= ) g (i, j) = add_edge g i j

  let ( <<= ) g s = add_edges g s

  let ( <!= ) g (i, j) = remove_edge g i j

  let ( <<!= ) g s = remove_edges g s

  let ( // ) g i = strip_white_node g i

  let ( /// ) g i = strip_white_node (set_color g i White) i
end
