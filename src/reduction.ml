(* NOTE : to obtain the "correct" implementation, all programs here must run in O(n) time. *)
(* Another graph implementation is definitely necessary. *)

(* O(n^2) *)
let nbcount_array (g : Graph.t) : int array =
  Array.init (Graph.len g) (fun i -> Graph.nb_neighbors g i)

(* false for white, true for black *)
let color_hash b w (len : int) : (int, bool) Hashtbl.t =
  let res = Hashtbl.create len in
  List.iter (fun x -> Hashtbl.replace res x true) b;
  List.iter (fun x -> Hashtbl.replace res x false) w;
  res

(* O(n^2) *)
(* let rec rule_1 (g : Graph.t) : Graph.t = g *)

let reduce_cautious (b : int list) (w : int list) (g : Graph.t) (k : int)
  (s : int list) : int list * int list * Graph.t * int * int list =
  (*
  let nbcounts = nbcount_array g in
  let colors = color_hash b w (Graph.len g) in
  *)
  (b, w, g, k, s)
