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

let define_t deg_ adj_ col_ = { deg = deg_; adj = adj_; col = col_ }

(* == Get functions == *)

let len g = Parray.length g.deg

let get_neighbors g i = Parray.get g.adj i

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
    begin
      match IntSet.find_opt j row with None -> false | Some _ -> true
    end

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

let rec add_edges (g : t) (es : (int * int) list) : t =
  match es with [] -> g | (i, j) :: es' -> add_edges (add_edge g i j) es'

let rec remove_edges (g : t) (es : (int * int) list) : t =
  match es with
  | [] -> g
  | (i, j) :: es' -> remove_edges (remove_edge g i j) es'

let set_color g i c : t =
  let newcol = Parray.set g.col i c in
  define_t g.deg g.adj newcol

(* == Other Functions specifically useful for the Algorithm == *)

(* Used in naive algorithm *)
let get_neighbors_list g i = IntSet.to_list @@ get_neighbors g i

(* Should be used in Rule 1 with vs the list of new white nodes *)
let rec remove_neighbors g i (vs : int list) =
  match vs with
  | [] -> g
  | v :: vs' -> remove_neighbors (remove_edge g i v) i vs'

(* Used multiple times when a node is "removed" *)
let ignore_node g i =
  let newdeg = Parray.set g.deg i 0 in
  let newadj = Parray.set g.adj i IntSet.empty in
  let newcol = Parray.set g.col i Null in
  define_t newdeg newadj newcol

(* t -> (t -> int -> t) -> t *)
let iter_on_nodes g f =
  let rec iter_ i g =
    begin
      match i with i' when i' = len g -> g | _ -> iter_ (i - 1) @@ f g i
    end
  in
  iter_ 0 g

(* == Functions for debugging == *)

let total_edges g = Parray.fold_left (fun i acc -> acc + i) 0 g.deg

let min_deg g =
  Parray.fold_left
    begin
      fun i acc -> if acc > i then i else acc
    end
    (len g) g.deg

let max_deg g =
  Parray.fold_left
    begin
      fun i acc -> if acc < i then i else acc
    end
    0 g.deg

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
