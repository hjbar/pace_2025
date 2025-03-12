(* Which algo is selected *)

type kind_algo =
  | BruteForce
  | Reduction

let kind_algo = Reduction

(* Which tests are selected *)

type kind_test =
  | Easy
  | Hard
  | Final
  | All

let kind_test = All

(* Perform visualisation ? *)

let do_visualisation = true
