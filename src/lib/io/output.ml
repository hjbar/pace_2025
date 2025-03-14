let read_output file =
  let in_c = open_in file in

  let rec parse flag card l =
    match In_channel.input_line in_c with
    | None -> (card, l)
    | Some s -> begin
      match String.split_on_char ' ' s with
      | c :: _ when c = "c" -> parse flag card l
      | [ n ] ->
        if flag then parse flag card ((int_of_string n - 1) :: l)
        else parse true (int_of_string n) l
      | _ -> failwith "Error in read_output : wrong format"
    end
  in

  let card, res = parse false 0 [] in
  close_in in_c;

  if card <> List.length res then
    failwith "Error in read_output : card <> |res|";

  res

let write_output file comment sol =
  let out_c = open_out file in

  let () =
    match comment with
    | None -> ()
    | Some c -> output_string out_c @@ Format.sprintf "c %s" c
  in
  output_string out_c "\n";

  List.length sol |> string_of_int |> output_string out_c;
  output_string out_c "\n";

  List.iter
    begin
      fun n ->
        n |> string_of_int |> output_string out_c;
        output_string out_c "\n"
    end
    sol;

  close_out out_c
