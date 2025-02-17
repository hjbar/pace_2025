(* NOTE : to obtain the "correct" implementation, all programs here must run in O(n) time. *)

(*

(* O(n^2) *)
let nbcount_array (g : Graph.t) : int array =
  Array.init (Graph.len g) (fun i -> Graph.nb_neighbors g i)

(* false for white, true for black *)
let color_hash b w (len : int) : (int, bool) Hashtbl.t =
  let res = Hashtbl.create len in
  List.iter (fun x -> Hashtbl.replace res x true) b;
  List.iter (fun x -> Hashtbl.replace res x false) w;
  res

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

(*
let rec rule_2 w g gi : unit =
  match w with
  | [] -> ()
  | u :: w' ->
    if gi.(u) = 1
      then 
*)

let reduce_cautious (b : int list) (w : int list) (g : Graph.t)
  (gi : int array) (wnew : int list) (k : int)
  (s : int list) : int list * int list * Graph.t * int * int list =
  
  (*
  let colors = color_hash b w (Graph.len g) in
  *)

  rule_1 wnew w g gi;

  (b, w, g, gi, k, s)

*)
