let () =
  Alcotest.run "Ch02"
    [ ("Persist", List_test.tests) (* ; "Tree", Tree_test.tests *) ]
