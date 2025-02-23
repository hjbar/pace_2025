(* Undirected unweighted (simple, in practice) graph *)

module IntSet = Set.Make (Int)

type color =
  | Black
  | White
  | Null

type t =
  { deg : int Parray.t
  ; adj : IntSet.t Parray.t
  ; col : color Parray.t
  }

(* Utility Functions *)

let init_empty (n : int) : t =
  { deg = Parray.init n (Fun.const 0)
  ; adj = Parray.init n (Fun.const IntSet.empty)
  ; col = Parray.init n (Fun.const Black)
  }

let define_t deg adj col = { deg; adj; col }

let copy g =
  let copy_parray a = Parray.init (Parray.length a) (fun i -> Parray.get a i) in
  define_t (copy_parray g.deg) (copy_parray g.adj) (copy_parray g.col)

(* == Get functions == *)

let len g = Parray.length g.deg

let get_neighbors g i = Parray.get g.adj i

let get_neighbors_and_self g i = IntSet.add i @@ Parray.get g.adj i

let get_degree g i = Parray.get g.deg i

let get_color g i = Parray.get g.col i

let is_white g i = get_color g i = White

let is_black g i = get_color g i = Black

let is_ignored g i = get_color g i = Null

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
    define_t newdeg newadj g.col

let remove_edge (g : t) i j : t =
  if not @@ is_edge g i j then g
  else
    let degi = Parray.get g.deg i in
    let degj = Parray.get g.deg j in
    let newrowi = IntSet.remove j @@ Parray.get g.adj i in
    let newrowj = IntSet.remove i @@ Parray.get g.adj j in

    let newdeg = Parray.set (Parray.set g.deg i (degi - 1)) j (degj - 1) in
    let newadj = Parray.set (Parray.set g.adj i newrowi) j newrowj in
    define_t newdeg newadj g.col

let add_edges (g : t) (es : (int * int) list) : t =
  List.fold_left (fun g (i, j) -> add_edge g i j) g es

let remove_edges (g : t) (es : (int * int) list) : t =
  List.fold_left (fun g (i, j) -> remove_edge g i j) g es

let set_color g i c : t =
  let newcol = Parray.set g.col i c in
  define_t g.deg g.adj newcol

let set_colors g vs c = List.fold_left (fun g i -> set_color g i c) g vs

let get_color_count c g =
  Parray.fold_left (fun acc c' -> if c' = c then acc + 1 else acc) 0 g.col

let get_blacknode_count = get_color_count Black

let get_whitenode_count = get_color_count White

(* == Other Utility == *)

(* Not quite a map, since a graph is not iterable *)
(* (t -> int -> t) -> t -> t *)
let map_like f g =
  let rec iter i g =
    begin
      match i with i' when i' = len g -> g | _ -> iter (i + 1) @@ f g i
    end
  in
  iter 0 g

(* (acc -> t -> int -> acc) -> acc -> t -> acc *)
let fold_left_like f init g =
  let rec fold i g acc =
    begin
      match i with
      | i' when i' = len g -> acc
      | _ -> fold (i + 1) g @@ f acc g i
    end
  in
  fold 0 g init

(* (t -> int -> t) -> (t -> int -> t) *)
let on_white f g i = if get_color g i = White then f g i else g

let on_black f g i = if get_color g i = Black then f g i else g

let on_deg d f g i = if get_degree g i = d then f g i else g

(* == Other Functions specifically useful for the Algorithm == *)

(* -- Used in naive algorithm -- *)
let get_neighbors_list g i = IntSet.elements @@ get_neighbors g i

(* Should be used in Rule 1 with vs the list of new white nodes *)
let remove_neighbors (vs : int list) g i =
  List.fold_left (fun g v -> remove_edge g i v) g vs

(* Used multiple times when a node is "removed" *)
let ignore_node g i =
  let newg = remove_neighbors (List.init (len g) Fun.id) g i in
  let newcol = Parray.set g.col i Null in
  define_t newg.deg newg.adj newcol

let min_deg_blacknode g =
  let r, _ =
    fold_left_like
      begin
        fun (node, deg) g i ->
          let d = get_degree g i in
          if is_black g i && deg > d then (i, d) else (node, deg)
      end
      (0, len g) (* We assume a simple graph *)
      g
  in
  r

let get_bw_inter_with_set g nv =
  IntSet.fold
    begin
      fun v (b, w) ->
        match get_color g v with
        | Black -> (v :: b, w)
        | White -> (b, v :: w)
        | Null -> failwith "Null-colored node should not have a neighbor"
    end
    nv ([], [])

(*
let nodes_over_4 g =
  let s = IntSet.of_list (List.init (len g) Fun.id) in
  fold_left_like
    begin
      fun s g i -> if get_degree g i < 4 then IntSet.remove i s else s
    end
    s g

(* Preprocesses deg(G'), with G' := G \ { v : deg_G(v) < 4 }. *)
let deg_over_4 g : int array =
  let arr = Array.init (len g) (Fun.const 0) in
  let nodes = nodes_over_4 g in
  let _, res =
    Parray.fold_left
      begin
        fun (i, res) nv ->
          let x = IntSet.cardinal @@ IntSet.inter nodes @@ nv in
          (i + 1, Array.set res i x)
      end
      (0, arr) g.adj
  in
  res
*)

(* Preprocesses G' := G \ { v : deg_G(v) < 4 }. *)
let g_over_4 g =
  fold_left_like
    begin
      fun g' g i -> if get_degree g i < 4 then ignore_node g' i else g'
    end
    g g

(* == Functions for debugging == *)

let total_edges g = Parray.fold_left (fun i acc -> acc + i) 0 g.deg

let min_deg g =
  Parray.fold_left (fun acc i -> if acc > i then i else acc) (len g) g.deg

let max_deg g =
  Parray.fold_left (fun acc i -> if acc < i then i else acc) 0 g.deg

let min_dom g : int =
  float (len g) /. (float (max_deg g) +. 1.) |> Float.ceil |> int_of_float

let max_dom g : int =
  let min_deg = min_deg g in
  if min_deg < 1 then failwith "MINIMUM DEGREE < 1";

  let cpt = ref 0. in

  for j = 1 to min_deg + 1 do
    cpt := (1. /. float j) +. !cpt
  done;

  int_of_float !cpt * len g / (min_deg + 1)

(* Notation for ease of use *)

module GraphNotation = struct
  let ( @? ) g (i, j) = is_edge g i j

  let ( <= ) g (i, j) = add_edge g i j

  let ( <<= ) g s = add_edges g s

  let ( <!= ) g (i, j) = remove_edge g i j

  let ( <<!= ) g s = remove_edges g s

  let ( // ) g i = ignore_node g i
end
