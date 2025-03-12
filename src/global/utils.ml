(* CONSTANTES *)

let dir_easy = "testset_easy"

let dir_hard = "testset_hard"

let dir_final = "testset_final"

let sep1 = String.make 30 '='

let sep2 = String.make 30 '-'

(* DIR *)

let compare_input s1 s2 =
  let cmp = compare (String.length s1) (String.length s2) in
  if cmp = 0 then compare s1 s2 else cmp

let get_gr testdir =
  testdir |> Sys.readdir |> Array.to_seq |> List.of_seq
  |> List.filter (fun filename -> Filename.extension filename = ".gr")
  |> List.map (fun filename -> Format.sprintf "%s/%s" testdir filename)
  |> List.sort compare_input

let get_sol testdir =
  testdir |> Sys.readdir |> Array.to_seq |> List.of_seq
  |> List.filter (fun filename -> Filename.extension filename = ".sol")
  |> List.map (fun filename -> Format.sprintf "%s/%s" testdir filename)
  |> List.sort compare_input

(* PRINT *)

let print_int_list l =
  let open Format in
  let f fmt l =
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_int fmt l
  in
  printf "[%a]@\n%!" f l

(* I/O *)

let open_out_trunc file =
  open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o666 file

let compile_graph file_dot file_pdf =
  Sys.command @@ Format.sprintf "dot -Tpdf %s -o %s" file_dot file_pdf |> ignore
