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
