open Globals

let () =
  if do_visualisation then Visualisation.get_all_pdf ();

  let algo =
    match kind_algo with
    | BruteForce -> Bruteforce.dominating
    | Reduction -> Reduction.dominating
  in

  match kind_test with
  | Easy -> Test.test_easy algo
  | Hard -> Test.test_hard algo
  | Final -> Test.test_final algo
  | All ->
    Test.test_easy algo;
    Test.test_hard algo;
    Test.test_final algo
