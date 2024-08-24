let () =
  Alcotest.run "Ch02"
    [ ("suffixes", List_test.Suffixes.tests) (* ; "Tree", Tree_test.tests *) ]
