(* IMPORT *)

open Utils

(* WRITE .DOT *)

let write_head out_c = output_string out_c "graph {\n\n"

let write_bot out_c = output_string out_c "\n}\n"

let write_body out_c graph =
  for i = 0 to Graph.len graph - 1 do
    for j = i to Graph.len graph - 1 do
      if graph.(i).(j) = 1 then begin
        output_string out_c @@ Format.sprintf "\t%d -- %d\n" (i + 1) (j + 1)
      end
    done
  done

(* GRAPH TO PDF *)

let get_pdf dir =
  List.iter
    begin
      fun path ->
        Format.printf "Get the pdf of the graph %s...@\n%!" path;

        let graph = Input.parse_input path in

        let filename = Filename.remove_extension path ^ ".dot" in
        let out_c = open_out_trunc filename in

        write_head out_c;
        write_body out_c graph;
        write_bot out_c;

        close_out out_c;

        compile_graph filename
    end
    (get_gr dir)

let get_all_pdf () =
  get_pdf dir_easy;
  get_pdf dir_hard
