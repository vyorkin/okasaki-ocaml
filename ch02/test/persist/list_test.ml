open Core
open Support
open Ch02.List

module Suffixes = struct
  let propLength l = List.(length l + 1 = length @@ suffixes l)
  let propHasEmptyListAsSuffix l = isSuffixOf ~eq:Int.equal ~suffix:[] l

  let test_number_of_suffixes =
    QCheck.Test.make ~count:50 ~name:"len(l) = n => len(suffixes(l)) = n + 1"
      QCheck.(list small_nat)
      propLength

  let test_empty_suffix =
    QCheck.Test.make ~count:30 ~name:"[] isSuffixOf suffixes(l)"
      QCheck.(list small_nat)
      propHasEmptyListAsSuffix

  let test_example () =
    Alcotest.(check (list (list int)))
      "bad suffixes"
      [ [ 1; 2; 3; 4 ]; [ 2; 3; 4 ]; [ 3; 4 ]; [ 4 ]; [] ]
      (suffixes [ 1; 2; 3; 4 ])

  let simple =
    let open Alcotest in
    [
      test_case "returns list of all suffixes in decreasing order of length"
        `Quick test_example;
    ]

  let qcheck =
    List.map ~f:QCheck_alcotest.to_alcotest
      [ test_number_of_suffixes; test_empty_suffix ]

  let tests = simple @ qcheck
end
