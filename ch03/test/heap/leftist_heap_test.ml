open Core
open Okasaki_common
open Ch03.Leftist_heap
module M = MkLeftistHeap (Ordered.Int)

let test_insert_single () =
  let heap = M.insert (1, M.empty) in
  let min_elem = M.find_min heap in
  Alcotest.(check (option int))
    "should insert single element and find_min it" (Some 1) min_elem

let test_insert_multiple () =
  let heap = M.insert (3, M.empty) in
  let heap = M.insert (5, heap) in
  let heap = M.insert (2, heap) in
  let min_elem = M.find_min heap in
  Alcotest.(check (option int))
    "should insert multiple elements and find_min" (Some 2) min_elem

let test_delete_min () =
  let heap = M.insert (3, M.empty) in
  let heap = M.insert (5, heap) in
  let heap = M.insert (2, heap) in
  let heap' = M.delete_min heap in
  let min_elem = Option.bind heap' ~f:M.find_min in
  Alcotest.(check (option int))
    "should delete minimum element" (Some 3) min_elem

let test_find_min_empty () =
  let min_elem = M.find_min M.empty in
  Alcotest.(check (option int)) "find_min in empty heap" None min_elem

let test_merge_heaps () =
  let heap1 = M.insert (3, M.empty) in
  let heap1 = M.insert (5, heap1) in
  let heap2 = M.insert (2, M.empty) in
  let heap2 = M.insert (4, heap2) in
  let merged_heap = M.merge (heap1, heap2) in
  let min_elem = M.find_min merged_heap in
  Alcotest.(check (option int)) "merge two heaps" (Some 2) min_elem

let test_delete_until_empty () =
  let heap = M.insert (3, M.empty) in
  let heap = M.insert (5, heap) in
  let heap = M.insert (2, heap) in
  let heap = M.insert (4, heap) in
  let rec delete_all h =
    match M.find_min h with
    | None -> true
    | Some _ -> delete_all (Option.value ~default:M.empty (M.delete_min h))
  in
  Alcotest.(check bool) "delete elements until empty" true (delete_all heap)

let tests =
  let open Alcotest in
  [
    test_case "insert single element" `Quick test_insert_single;
    test_case "insert multiple elements" `Quick test_insert_multiple;
    test_case "delete minimum element" `Quick test_delete_min;
    test_case "find min in empty heap" `Quick test_find_min_empty;
    test_case "merge two heaps" `Quick test_merge_heaps;
    test_case "delete elements until empty" `Quick test_delete_until_empty;
  ]
