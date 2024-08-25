open Core
open Support
open Okasaki_common
open Ch02.Unbalanced_set
module M = MkUnbalancedSet (Ordered.Int)

let tree_testable = M.(Alcotest.testable pp equal)

let test_member_for_empty_tree () =
  let tree = M.empty in
  Alcotest.(
    check bool "member 1 should not be found in an empty tree" false
      (M.member (1, tree)))

let test_member_for_single_element_tree () =
  let tree = M.(node empty 5 empty) in
  Alcotest.(
    check bool "member 5 found in single element tree" true (M.member (5, tree)));
  Alcotest.(
    check bool "member 3 should not be found in single element tree" false
      (M.member (3, tree)))

let test_member_for_balanced_tree () =
  let tree = M.(node (node empty 2 empty) 4 (node empty 7 empty)) in
  Alcotest.(check bool "member 7 not found" true (M.member (7, tree)))

let test_member_for_unbalanced_tree () =
  let tree = M.(node (node empty 7 empty) 4 (node empty 2 empty)) in
  Alcotest.(
    check bool "member 7 should not be found" false (M.member (7, tree)))

let test_member_for_absent_element () =
  let tree = M.(node (node empty 1 empty) 3 (node empty 5 empty)) in
  Alcotest.(
    check bool "member 4 should not be found" false (M.member (4, tree)))

let test_member_for_right_heavy_tree () =
  let tree = M.(node empty 3 (node empty 6 (node empty 9 empty))) in
  Alcotest.(
    check bool "member 6 found in right-heavy tree" true (M.member (6, tree)));
  Alcotest.(
    check bool "member 2 should not be found in right-heavy tree" false
      (M.member (2, tree)))

let test_member_for_left_heavy_tree () =
  let tree = M.(node (node (node empty 1 empty) 2 empty) 3 empty) in
  Alcotest.(
    check bool "member 1 found in left-heavy tree" true (M.member (1, tree)));
  Alcotest.(
    check bool "member 4 should not be found in left-heavy tree" false
      (M.member (4, tree)))

let cases =
  let open Alcotest in
  [
    test_case "member works for balanced tree" `Quick
      test_member_for_balanced_tree;
    test_case "member doesn't work for unbalanced tree" `Quick
      test_member_for_unbalanced_tree;
    test_case "member returns false for empty tree" `Quick
      test_member_for_empty_tree;
    test_case "member works for single element tree" `Quick
      test_member_for_single_element_tree;
    test_case "member returns false for absent element" `Quick
      test_member_for_absent_element;
    test_case "member works for right-heavy tree" `Quick
      test_member_for_right_heavy_tree;
    test_case "member works for left-heavy tree" `Quick
      test_member_for_left_heavy_tree;
  ]

let tests = cases
