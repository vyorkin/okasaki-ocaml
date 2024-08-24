open Core
open Support
open Okasaki_common
open Ch02.Unbalanced_set
module M = MkUnbalancedSet (Ordered.Int)

let tree_testable = M.(Alcotest.testable pp equal)

let test_member_for_balanced_tree () =
  let tree = M.(node (node empty 2 empty) 4 (node empty 7 empty)) in
  Alcotest.(
    check bool "member 7 not found in balanced tree" true (M.member (7, tree)))

let test_member_for_unbalanced_tree () =
  let tree = M.(node (node empty 7 empty) 4 (node empty 2 empty)) in
  Alcotest.(
    check bool "member 7 found in unbalanced tree" false (M.member (7, tree)))

let cases =
  let open Alcotest in
  [
    test_case "member works for balanced tree" `Quick
      test_member_for_balanced_tree;
    test_case "member doesn't work for unbalanced tree" `Quick
      test_member_for_unbalanced_tree;
  ]

let tests = cases
