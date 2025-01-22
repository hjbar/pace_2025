(* CONSTANTES *)

let dir_easy = "testset_easy"

let dir_hard = "testset_hard"

let sep1 = String.make 30 '='

let sep2 = String.make 30 '-'

(* UTILS *)

let get_gr testdir =
  testdir |> Sys.readdir |> Array.to_seq |> List.of_seq
  |> List.filter (fun filename -> Filename.extension filename = ".gr")
  |> List.map (fun filename -> Format.sprintf "%s/%s" testdir filename)
  |> List.sort compare

let get_sol testdir =
  testdir |> Sys.readdir |> Array.to_seq |> List.of_seq
  |> List.filter (fun filename -> Filename.extension filename = ".sol")
  |> List.map (fun filename -> Format.sprintf "%s/%s" testdir filename)
  |> List.sort compare

let print_int_list l =
  let open Format in
  let f fmt l =
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_int fmt l
  in
  printf "[%a]@\n%!" f l

(* TESTING *)

let test_gen kind testdir =
  Format.printf "%s@\n%!" sep1;
  Format.printf "Testing %s : %!" kind;

  let ok =
    List.fold_left2
      begin
        fun flag file_gr file_sol ->
          let graph = Input.parse_input file_gr in
          let res = Dominating.dominating graph |> List.sort compare in

          let sol = Output.read_output file_sol |> List.sort compare in

          if res <> sol then begin
            if flag then Format.printf "@\n@\n@\n%!";
            Format.printf "%s@\n%!" sep2;

            Format.printf "Error in test of %s@\n%!"
            @@ Filename.remove_extension file_gr;

            Format.printf "Result : %!";
            print_int_list res;

            Format.printf "@\nSolution : %!";
            print_int_list sol;

            Format.printf "%s@\n@\n%!" sep2;

            false
          end
          else flag
      end
      true (get_gr testdir) (get_sol testdir)
  in

  if ok then Format.printf "OK@\n%s@\n%!" sep1
  else Format.printf "@\n@\nTesting %s : ERROR@\n%s@\n%!" kind sep1

let test_easy () = test_gen "easy" dir_easy

let test_hard () = test_gen "hard" dir_hard
