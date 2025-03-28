(* IMPORT *)

open Utils

(* TESTING *)

let ( !+ ) l = List.map (fun i -> i + 1) l

let test_gen kind testdir algo =
  Format.printf "%s@\n%!" sep1;
  Format.printf "Testing %s : @\n@\n%!" kind;

  let ok =
    List.fold_left2
      begin
        fun flag file_gr file_sol ->
          let file_out = Filename.chop_extension file_sol ^ ".out" in
          if Sys.file_exists file_out then flag
          else begin
            Format.printf "Instance %s...@\n%!" file_gr;

            let graph = Input.parse_input file_gr in
            let res = algo graph |> List.sort compare in

            let sol = Output.read_output file_sol |> List.sort compare in

            let flag =
              if res = sol then flag
              else begin
                if flag then Format.printf "@\n@\n@\n%!";
                Format.printf "%s@\n%!" sep2;

                Format.printf "Result differs from Solution in test of %s@\n%!"
                @@ Filename.remove_extension file_gr;

                if List.length res = List.length sol then
                  Format.printf "@\nHowever, they are of the same size@\n";

                Format.printf "Result :   %!";
                print_int_list !+res;

                Format.printf "@\nSolution : %!";
                print_int_list !+sol;

                Format.printf "%s@\n@\n%!" sep2;

                false
              end
            in

            let comment =
              if flag then "success"
              else if List.length res = List.length sol then "probably success"
              else "failed"
            in
            Output.write_output file_out (Some comment) sol;

            flag
          end
      end
      true (get_gr testdir) (get_sol testdir)
  in

  if ok then Format.printf "@\nOK@\n%s@\n%!" sep1
  else Format.printf "@\n@\nTesting %s : ERROR@\n%s@\n%!" kind sep1

let test_easy = test_gen "easy" dir_easy

let test_hard = test_gen "hard" dir_hard

let test_final = test_gen "final" dir_final
