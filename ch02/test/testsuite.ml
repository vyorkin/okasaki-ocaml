let () =
  Alcotest.run "Ch02"
    [
      ("suffixes", List_test.Suffixes.tests);
      ("unbalanced set", Unbalanced_set_test.tests);
      ("finite map", Finite_map_test.tests);
    ]
