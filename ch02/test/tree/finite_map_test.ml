open Core
open Support
open Okasaki_common
open Ch02.Finite_map
module M = MkFiniteMap (Ordered.Int) (Ordered.Int)

let test_insert_into_empty_map () =
  let map = M.empty in
  let map' = M.bind (1, 10, map) in
  let result = M.lookup (1, map') in
  Alcotest.(check int) "lookup key after insertion" 10 result

let test_insert_into_single_element_map () =
  let map = M.bind (1, 10, M.empty) in
  let map' = M.bind (2, 20, map) in
  let result1 = M.lookup (1, map') in
  let result2 = M.lookup (2, map') in
  Alcotest.(check int) "value 10 should be found by key 1" 10 result1;
  Alcotest.(check int) "value 20 should be found by key 2" 20 result2

let test_insert_duplicate_key () =
  let map = M.bind (1, 10, M.empty) in
  let map' = M.bind (1, 100, map) in
  let result = M.lookup (1, map') in
  Alcotest.(check int)
    "original value is preserved after inserting duplicate key" 10 result

let test_lookup_key () =
  let map = M.bind (1, 10, M.empty) in
  let result = M.lookup (1, map) in
  Alcotest.(check int) "should find a vault by existing key" 10 result

let test_lookup_non_existing_key () =
  let map = M.bind (1, 10, M.empty) in
  Alcotest.check_raises "should fail to find value by non-existing key"
    M.NotFoundError (fun () -> ignore (M.lookup (2, map)))

let bind_tests =
  let open Alcotest in
  [
    test_case "insert into empty map" `Quick test_insert_into_empty_map;
    test_case "insert into single-element map" `Quick
      test_insert_into_single_element_map;
    test_case "insert duplicate key" `Quick test_insert_duplicate_key;
  ]

let lookup_tests =
  let open Alcotest in
  [
    test_case "lookup existing key" `Quick test_lookup_key;
    test_case "lookup non-existing key" `Quick test_lookup_non_existing_key;
  ]

let tests = bind_tests @ lookup_tests
