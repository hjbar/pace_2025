
(* Undirected unweighted (simple, in practice) graph *)

module IntSet = SetMake(int) ;;

type t = IntSet Parray.t ;;

(* Utility Functions *)

(* Initializes an empty graph *)
let init (n : int) : t = Parray.init n (Fun.const IntSet.empty)

let len (g : t) = Parray.length t

(* == Get functions == *)

let get_neighbors (g : t) i = Parray.get g i

let get_degree (g : t) i = IntSet.cardinal @@ Parray.get g i

let get_edge (g : t) i j =
  let row = Parray.get g i in
  begin match IntSet.find_opt row j with
  | None -> 0
  | Some _ -> 1
  end

let ( => ) (g : t) (m, n : int * int) : int = get_edge g m n

(* == Modifications == *)

let add_edge (g : t) i j : t =
  let rowi', rowj' = Intset.add (Parray.get g i) j, Intset.add (Parray.get g j) i in
  Parray.set (Parray.set g j rowj') i rowi'

let ( <= ) (g : t) (m, n : int * int) : t = add_edge g m n

(* == Functions for debugging == *)

let total_edges (g : t) =
  Parray.fold_left
    begin
      fun s acc = acc + (IntSet.cardinal s)
    end
    g 0

(*
type t = int array array

(* Fonctions utilitaires *)

let len (g : t) = Array.length g

let neighbors (g : t) (v : int) =
  let _, res =
    Array.fold_left
      begin
        fun (i, acc) n ->
          if v <> i && n = 1 then (i + 1, i :: acc) else (i + 1, acc)
      end
      (0, []) g.(v)
  in
  res

let nb_neighbors (g : t) (v : int) : int =
  Array.fold_left (fun acc n -> if n = 1 then acc + 1 else acc) 0 g.(v)

let nb_edges (g : t) : int =
  Array.fold_left
    (fun acc t ->
      Array.fold_left (fun acc n -> if n = 1 then acc + 1 else acc) acc t )
    0 g

let min_deg (g : t) : int =
  let min_deg = ref (len g) in

  for i = 0 to Array.length g - 1 do
    let cur_deg = ref 0 in

    for j = 0 to Array.length g - 1 do
      if g.(i).(j) = 1 then incr cur_deg
    done;

    if !cur_deg < !min_deg then min_deg := !cur_deg
  done;

  !min_deg

let max_deg (g : t) : int =
  let max_deg = ref 0 in

  for i = 0 to Array.length g - 1 do
    let cur_deg = ref 0 in

    for j = 0 to Array.length g - 1 do
      if g.(i).(j) = 1 then incr cur_deg
    done;

    if !cur_deg > !max_deg then max_deg := !cur_deg
  done;

  !max_deg

let min_dom (g : t) : int =
  float (len g) /. (float (max_deg g) +. 1.) |> Float.ceil |> int_of_float

let max_dom (g : t) : int =
  let min_deg = min_deg g in
  if min_deg < 1 then failwith "MINIUM DEGREE < 1";

  let cpt = ref 0. in

  for j = 1 to min_deg + 1 do
    cpt := (1. /. float j) +. !cpt
  done;

  int_of_float !cpt * len g / (min_deg + 1)
*)