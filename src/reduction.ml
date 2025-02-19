(* NOTE : to obtain the "correct" implementation, all programs here must run in O(n) time. *)

(* Rules 1 and 2 can be applied before all the other rules *)
(* Rules >1 cannot trigger Rule 1, and Rules >2 cannot trigger Rule 2 *)

let rule_1 wnew g =
  Graph.map_like (Graph.on_white @@ Graph.remove_neighbors wnew) g

let rule_2 g = g

(* Below implementation is wrong, a simple pass isn't enough *)
(*
let rule_2 g =
  let delete_hanging g i =
    if Graph.get_degree g i = 1 then Graph.ignore_node g i else g
  in
  Graph.map_like (Graph.on_white delete_hanging) g
*)

(* Rule 3 is optional *)
(* Rule 3 can trigger Rules 4, 5', 6', 7 and be triggered by them.*)
(* If implemented, maybe run it twice, once before rules 4-7 and once after *)

let rule_3 g k s = (g, k, s)

(* I believe rules 4 to 7 cannot trigger each other or themselves *)

let rule_4 g = g

let rule_5' g = g

let rule_6' g = g

let rule_7 g = g

let reduce_cautious (g : Graph.t) (wnew : int list) (k : int) (s : int list) :
  Graph.t * int * int list =
  let g = g |> rule_1 wnew |> rule_2 in

  let g, k, s = rule_3 g k s in

  let g = g |> rule_4 |> rule_5' |> rule_6' |> rule_7 in

  rule_3 g k s
