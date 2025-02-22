(* Which algo is selected *)

type kind_algo =
  | BruteForce
  | Reduction

let kind_algo = Reduction

(* Which tests are selected *)

type kind_test =
  | Easy
  | Hard
  | All

let kind_test = Easy

(* Perform visualisation ? *)

let do_visualisation = true
