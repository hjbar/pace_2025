(* Undirected unweighted (simple, in practice) graph *)

module IntSet = Set.Make (Int)

(* Black vertices are non-dominated *)
(* White vertices are dominated *)

type color =
  | Black
  | White

type t =
  { deg : int Parray.t
  ; adj : IntSet.t Parray.t
  ; col : color Parray.t
  ; nb_b : int
  ; degcount : int Parray.t
  }

(* Utility Functions *)

let init_empty n =
  { deg = Parray.init n (Fun.const 0)
  ; adj = Parray.init n (Fun.const IntSet.empty)
  ; col = Parray.init n (Fun.const Black)
  ; nb_b = n
  ; degcount = Parray.init n (fun i -> if i = 0 then n else 0)
  }

let define_t deg adj col nb_b degcount = { deg; adj; col; nb_b; degcount }

let copy g =
  let copy_parray a = Parray.init (Parray.length a) (Parray.get a) in
  define_t (copy_parray g.deg) (copy_parray g.adj) (copy_parray g.col) g.nb_b
    (copy_parray g.degcount)

let update_degcount maxdeg g =
  define_t g.deg g.adj g.col g.nb_b
  @@ Parray.init (maxdeg + 1) (Parray.get g.degcount)

(* == Get functions == *)

let len g = Parray.length g.deg

let get_neighbors g i = Parray.get g.adj i

let get_neighbors_and_self g i = IntSet.add i @@ Parray.get g.adj i

let get_degree g i = Parray.get g.deg i

let get_color g i = Parray.get g.col i

let is_white g i = get_color g i = White

let is_black g i = get_color g i = Black

let get_blacknode_count g = g.nb_b

let is_singleton g i = Parray.get g.deg i = 0

let is_edge g i j =
  if is_singleton g i || is_singleton g j then false
  else
    let row = Parray.get g.adj i in
    IntSet.find_opt j row |> Option.is_some

(* == Modifications == *)

let add_edge g i j =
  if is_edge g i j then g
  else
    let degi = Parray.get g.deg i in
    let degj = Parray.get g.deg j in
    let newrowi = IntSet.add j @@ Parray.get g.adj i in
    let newrowj = IntSet.add i @@ Parray.get g.adj j in

    let newdeg = Parray.set (Parray.set g.deg i (degi + 1)) j (degj + 1) in
    let newadj = Parray.set (Parray.set g.adj i newrowi) j newrowj in
    let newdegcount' =
      Parray.set
        (Parray.set g.degcount degi (Parray.get g.degcount degi - 1))
        (degi + 1)
        (Parray.get g.degcount (degi + 1) + 1)
    in
    let newdegcount =
      Parray.set
        (Parray.set newdegcount' degj (Parray.get newdegcount' degj - 1))
        (degj + 1)
        (Parray.get newdegcount' (degj + 1) + 1)
    in
    define_t newdeg newadj g.col g.nb_b newdegcount

let remove_edge g i j =
  if not @@ is_edge g i j then g
  else
    let degi = Parray.get g.deg i in
    let degj = Parray.get g.deg j in
    let newrowi = IntSet.remove j @@ Parray.get g.adj i in
    let newrowj = IntSet.remove i @@ Parray.get g.adj j in

    let newdeg = Parray.set (Parray.set g.deg i (degi - 1)) j (degj - 1) in
    let newadj = Parray.set (Parray.set g.adj i newrowi) j newrowj in
    let newdegcount' =
      Parray.set
        (Parray.set g.degcount degi (Parray.get g.degcount degi - 1))
        (degi - 1)
        (Parray.get g.degcount (degi - 1) + 1)
    in
    let newdegcount =
      Parray.set
        (Parray.set newdegcount' degj (Parray.get newdegcount' degj - 1))
        (degj - 1)
        (Parray.get newdegcount' (degj - 1) + 1)
    in
    define_t newdeg newadj g.col g.nb_b newdegcount

let add_edges g es = List.fold_left (fun g (i, j) -> add_edge g i j) g es

let remove_edges g es = List.fold_left (fun g (i, j) -> remove_edge g i j) g es

let set_color g i c =
  if Parray.get g.col i = c then g
  else
    let newcol = Parray.set g.col i c in
    define_t g.deg g.adj newcol
      (if c = Black then g.nb_b + 1 else g.nb_b - 1)
      g.degcount

let set_colors g vs c = IntSet.fold (fun i g -> set_color g i c) vs g

(* == Reduction Algorithm == *)

(* Used multiple times when a node is "removed" *)
let strip_white_node g i =
  let g =
    IntSet.fold
      begin
        fun n g ->
          let degn = Parray.get g.deg n in
          define_t
            (Parray.set g.deg n (degn - 1))
            (Parray.set g.adj n @@ IntSet.remove i @@ Parray.get g.adj n)
            g.col g.nb_b
            (Parray.set
               (Parray.set g.degcount degn (Parray.get g.degcount degn - 1))
               (degn - 1)
               (Parray.get g.degcount (degn - 1) + 1) )
      end
      (get_neighbors g i) g
  in

  let newdeg = Parray.set g.deg i 0 in
  let newadj = Parray.set g.adj i IntSet.empty in
  let degi = Parray.get g.deg i in
  let newdegcount =
    if degi = 0 then g.degcount
    else
      Parray.set
        (Parray.set g.degcount degi (Parray.get g.degcount degi - 1))
        0
        (Parray.get g.degcount 0 + 1)
  in

  define_t newdeg newadj g.col g.nb_b newdegcount

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

let max_possible_domination g k =
  let rec aux i k acc =
    let dc = Parray.get g.degcount i in
    if dc >= k then acc + (k * (i + 1))
    else aux (i - 1) (k - dc) (acc + (dc * (i + 1)))
  in

  aux (Parray.length g.degcount - 1) k 0

(* == Function for min_deg == *)

let min_deg g =
  Parray.fold_left (fun acc i -> if acc > i then i else acc) (len g) g.deg

(* == Function for max_deg == *)

let max_deg g =
  Parray.fold_left (fun acc i -> if acc < i then i else acc) 0 g.deg

(* == Functions for min_dom == *)

let min_dom_b1 ~len ~max_deg =
  float len /. float (max_deg + 1) |> Float.floor |> int_of_float

let min_dom_b2 ~g ~len =
  let rec aux i =
    if max_possible_domination g i >= len then i else aux (i + 1)
  in
  aux 1

let min_dom g =
  let len = len g in
  let max_deg = max_deg g in

  let b1 = min_dom_b1 ~len ~max_deg in
  let b2 = min_dom_b2 ~g ~len in

  List.fold_left max min_int [ b1; b2 ]

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

let max_dom_b10 ~g =
  let g = copy g in

  let get_max_deg g =
    let _, res, _ =
      Parray.fold_left
        begin
          fun (cur_i, max_i, max_deg) deg ->
            if max_deg <= deg && get_color g cur_i = Black then
              (cur_i + 1, cur_i, deg)
            else (cur_i + 1, max_i, max_deg)
        end
        (0, 0, 0) g.deg
    in
    res
  in

  let rec loop g sol =
    if g.nb_b = 0 then sol
    else
      let neigh = get_neighbors_and_self g @@ get_max_deg g in

      let g =
        IntSet.fold
          (fun i g -> strip_white_node g i)
          neigh (set_colors g neigh White)
      in

      loop g (sol + 1)
  in

  loop g 0

let max_dom g =
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
  let b10 = max_dom_b10 ~g in

  List.fold_left min max_int [ b1; b2; b3; b4; b5; b6; b7; b8; b9; b10 ]

(* == Functions for debugging == *)

let total_edges g = Parray.fold_left (fun i acc -> acc + i) 0 g.deg

let s_degcount g s =
  let s =
    s ^ ": ["
    ^ Parray.fold_left (fun acc i -> acc ^ string_of_int i ^ "; ") "" g.degcount
    ^ "] sums to "
    ^ string_of_int (Parray.fold_left (fun acc i -> acc + i) 0 g.degcount)
    ^ "\n"
  in
  s

(* Notation for ease of use *)

module GraphNotation = struct
  let ( @? ) g (i, j) = is_edge g i j

  let ( <== ) g (i, j) = add_edge g i j

  let ( <<== ) g s = add_edges g s

  let ( <!== ) g (i, j) = remove_edge g i j

  let ( <<!== ) g s = remove_edges g s

  let ( // ) g i = strip_white_node g i

  let ( /// ) g i = strip_white_node (set_color g i White) i
end

(* == Only in Naive Algorithm == *)

let get_neighbors_list g i = IntSet.elements @@ get_neighbors g i
