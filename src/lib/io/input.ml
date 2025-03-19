open Graph.GraphNotation

let parse_input file : Graph.t =
  let in_c = open_in file in

  let rec parse vertex edges assoc =
    match In_channel.input_line in_c with
    | None -> (vertex, edges, assoc)
    | Some s -> begin
      match String.split_on_char ' ' s with
      | c :: _ when c = "c" -> parse vertex edges assoc
      | [ p; ds; n1; n2 ] when p = "p" && ds = "ds" ->
        parse (int_of_string n1) (int_of_string n2) assoc
      | [ n1; n2 ] ->
        let v1 = int_of_string n1 - 1 in
        let v2 = int_of_string n2 - 1 in
        parse vertex edges ((v1, v2) :: assoc)
      | _ -> failwith "Error in parse_input : wrong format"
    end
  in

  let nb_vertex, nb_edges, assoc = parse 0 0 [] in
  close_in in_c;

  if nb_edges <> List.length assoc then
    failwith "Error in parse_input : nb_edges <> |assoc|";

  let graph = Graph.init_empty nb_vertex in
  graph <<== assoc
