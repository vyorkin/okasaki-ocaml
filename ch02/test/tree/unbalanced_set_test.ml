open Core
open Support
open Okasaki_common
open Ch02.Unbalanced_set
module M = MkUnbalancedSet (Ordered.Int)

let tree_testable = M.(Alcotest.testable pp equal)

let mk_member_tests ~name f =
  let test_empty_tree () =
    let tree = M.empty in
    Alcotest.(
      check bool "member 1 should not be found in an empty tree" false
        (f (1, tree)))
  in
  let test_single_element_tree () =
    let tree = M.(node empty 5 empty) in
    Alcotest.(
      check bool "member 5 found in single element tree" true (f (5, tree)));
    Alcotest.(
      check bool "member 3 should not be found in single element tree" false
        (f (3, tree)))
  in
  let test_balanced_tree () =
    let tree = M.(node (node empty 2 empty) 4 (node empty 7 empty)) in
    Alcotest.(check bool "member 7 not found" true (f (7, tree)))
  in
  let test_unbalanced_tree () =
    let tree = M.(node (node empty 7 empty) 4 (node empty 2 empty)) in
    Alcotest.(check bool "member 7 should not be found" false (f (7, tree)))
  in
  let test_absent_element () =
    let tree = M.(node (node empty 1 empty) 3 (node empty 5 empty)) in
    Alcotest.(check bool "member 4 should not be found" false (f (4, tree)))
  in
  let test_right_heavy_tree () =
    let tree = M.(node empty 3 (node empty 6 (node empty 9 empty))) in
    Alcotest.(
      check bool "member 6 found in right-heavy tree" true (f (6, tree)));
    Alcotest.(
      check bool "member 2 should not be found in right-heavy tree" false
        (f (2, tree)))
  in
  let test_left_heavy_tree () =
    let tree = M.(node (node (node empty 1 empty) 2 empty) 3 empty) in
    Alcotest.(check bool "member 1 found in left-heavy tree" true (f (1, tree)));
    Alcotest.(
      check bool "member 4 should not be found in left-heavy tree" false
        (f (4, tree)))
  in
  let open Alcotest in
  [
    test_case (name ^ " works for balanced tree") `Quick test_balanced_tree;
    test_case
      (name ^ " doesn't work for unbalanced tree")
      `Quick test_unbalanced_tree;
    test_case (name ^ " returns false for empty tree") `Quick test_empty_tree;
    test_case
      (name ^ " works for single element tree")
      `Quick test_single_element_tree;
    test_case
      (name ^ " returns false for absent element")
      `Quick test_absent_element;
    test_case
      (name ^ " works for right-heavy tree")
      `Quick test_right_heavy_tree;
    test_case (name ^ " works for left-heavy tree") `Quick test_left_heavy_tree;
  ]

let mk_insert_tests ~name f =
  let test_insert_into_empty_tree () =
    let tree = M.empty in
    let expected = M.(node empty 5 empty) in
    Alcotest.(
      check tree_testable "failed to insert 5 into empty tree" expected
        (f (5, tree)))
  in
  let test_insert_into_single_element_tree () =
    let tree = M.(node empty 5 empty) in
    let expected = M.(node (node empty 3 empty) 5 empty) in
    Alcotest.(
      check tree_testable "failed to insert 3 into tree with one element"
        expected
        (f (3, tree)))
  in
  let test_insert_unbalancing_value () =
    let tree = M.(node (node empty 3 empty) 5 empty) in
    let expected = M.(node (node empty 3 empty) 5 (node empty 7 empty)) in
    Alcotest.(
      check tree_testable "failed to insert 7 into unbalanced tree" expected
        (f (7, tree)))
  in
  let test_insert_duplicate_value () =
    let tree = M.(node (node empty 3 empty) 5 empty) in
    Alcotest.(
      check tree_testable "failed to insert duplicate 3 into tree" tree
        (f (3, tree)))
  in
  let test_insert_multiple_values () =
    let tree = M.empty in
    let tree = f (5, tree) in
    let tree = f (3, tree) in
    let tree = f (7, tree) in
    let tree = f (2, tree) in
    let tree = f (6, tree) in
    let expected =
      M.(
        node
          (node (node empty 2 empty) 3 empty)
          5
          (node (node empty 6 empty) 7 empty))
    in
    Alcotest.(
      check tree_testable "failed to insert multiple values into tree" expected
        tree)
  in
  let test_insert_existing_value () =
    let tree =
      M.(
        node
          (node (node empty 2 empty) 3 empty)
          5
          (node (node empty 6 empty) 7 empty))
    in
    Alcotest.(
      check tree_testable "failed to insert existing value 3 into tree" tree
        (f (3, tree)))
  in
  let open Alcotest in
  [
    test_case
      (name ^ ": insert into empty tree")
      `Quick test_insert_into_empty_tree;
    test_case
      (name ^ ": insert into single element tree")
      `Quick test_insert_into_single_element_tree;
    test_case
      (name ^ ": insert unbalancing value")
      `Quick test_insert_unbalancing_value;
    test_case
      (name ^ ": insert duplicate value")
      `Quick test_insert_duplicate_value;
    test_case
      (name ^ ": insert multiple values")
      `Quick test_insert_multiple_values;
    test_case
      (name ^ ": existing value in a non-trivial tree")
      `Quick test_insert_existing_value;
  ]

let tests =
  mk_member_tests ~name:"member" M.member
  @ mk_member_tests ~name:"member_ex_2_2" M.member_ex_2_2
  @ mk_insert_tests ~name:"insert" M.insert
  @ mk_insert_tests ~name:"insert_ex_2_3" M.insert_ex_2_3
  @ mk_insert_tests ~name:"insert_ex_2_4" M.insert_ex_2_4
