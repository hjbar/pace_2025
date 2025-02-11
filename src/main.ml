let () =
  Visualisation.get_all_pdf ();

  Test.test_easy ();
  Test.test_hard ()
