(* Une matrice n x n + les valeurs de la diagonale sont 1 *)

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

let nb_egdes (g : t) : int =
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
