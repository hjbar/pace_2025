(* NOTE : to obtain the "correct" implementation, all programs here must run in O(n) time. *)

(*
(* O(n^2) but expected complexity O(6n) *)
let rec rule_1 wnew w g gi : unit =
  match wnew with
  | [] -> ()
  | u :: wnew' ->
    List.iter
      begin
        fun v ->
          let is_edge = g.(u).(v) in
          (
            if is_edge 
              then
                gi.(u) <- gi.(u) - 1;
                gi.(v) <- gi.(v) - 1
          );
          g.(u).(v) <-0;
          g.(v).(u) <- 0
      end w;
    rule_1 wnew' w g gi


let rec rule_2 w g gi : unit =
  match w with
  | [] -> ()
  | u :: w' ->
    if gi.(u) = 1
      then 
*)

let do_nothing wnew = match wnew with _ -> ()

let reduce_cautious (g : Graph.t) (wnew : int list) (k : int) (s : int list) :
  Graph.t * int * int list =
  do_nothing wnew;
  (g, k, s)

(*rule_1 wnew w g gi;*)
