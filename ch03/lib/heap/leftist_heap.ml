open Core
open Okasaki_common

module MkLeftistHeap (E : Ordered.S) : Heap_intf.S with type elem = E.t = struct
  type elem = E.t [@@deriving eq, show { with_path = false }]

  type t = E | T of int * elem * t * t
  [@@deriving eq, show { with_path = false }]

  let empty = E
  let node rank x l r = T (rank, x, l, r)
  let leaf x = T (1, x, E, E)
  let is_empty = function E -> true | _ -> false
  let rank = function E -> 0 | T (r, _, _, _) -> r

  (* Creates a new node, calculates the rank of `l` node and swaps its nodes if necessary.
     The rank of any left child should be at least as large as the rank of its right sibling. *)
  let mkT (x, l, r) =
    let rl, rr = (rank l, rank r) in
    if rl >= rr then T (rl + 1, x, l, r) else T (rr + 1, x, r, l)

  let rec merge = function
    | h1, E -> h1
    | E, h2 -> h2
    | (T (_, x, l1, r1) as h1), (T (_, y, l2, r2) as h2) ->
        if E.(x <= y) then mkT (x, l1, merge (r1, h2))
        else mkT (y, l2, merge (h1, r2))

  let insert (x, h) = merge (T (1, x, E, E), h)
  let delete_min = function E -> None | T (_, _, l, r) -> Some (merge (l, r))
  let find_min = function E -> None | T (_, x, _, _) -> Some x

  let insert_3_2 (x, t) =
    let rec insert (x, t) =
      match t with
      | E -> leaf x
      | T (_, y, l, r) ->
          (* The leftist property will hold if we always insert into the left subtree. *)
          if E.(x <= y) then mkT (x, insert (y, l), r)
          else mkT (y, insert (x, l), r)
    in
    insert (x, t)

  let from_list_fold_right l =
    let f x acc = merge (leaf x, acc) in
    List.fold_right l ~init:empty ~f

  (* Ex. 3.3: Should have O(n) complexity. *)
  let from_list l =
    let heaps = List.map l ~f:leaf in
    let rec merge_adjacent = function
      | h1 :: h2 :: hs -> merge (h1, h2) :: merge_adjacent hs
      | hs -> hs
    in
    let rec fold = function
      | [] -> empty
      | [ h ] -> h
      | hs -> fold (merge_adjacent hs)
    in
    fold heaps

  (* Turning each element into a heap is O(1) => this step is O(n).
     Number of heaps is halved in each pass, and each pass involves O(n) work,
     the total number of passes is O(log n). However, since each pass only processes
     half the heaps as the previous one, the overall complexity remains O(n). *)
end

(* Ex. 3.4 (b) *)
module MkWeightBiasedLeftistHeap (E : Ordered.S) :
  Heap_intf.S with type elem = E.t = struct
  type elem = E.t [@@deriving eq, show { with_path = false }]

  type t = E | T of int * elem * t * t
  [@@deriving eq, show { with_path = false }]

  let empty = E
  let leaf x = T (1, x, E, E)
  let is_empty = function E -> true | _ -> false
  let size = function E -> 0 | T (s, _, _, _) -> s

  (* Ex. 3.4 (c)
     Currently, merge operates in two passes:
     1) a top-down pass consisting of calls to `merge`, and
     2) a bottom-up pass consisting of calls to the helper function `mkT`.
     Modify `merge` to operate in a single, top-down pass. *)
  let rec merge = function
    | h1, E -> h1
    | E, h2 -> h2
    | (T (_, x, l1, r1) as h1), (T (_, y, l2, r2) as h2) ->
        if E.(x <= y) then
          let h = merge (r1, h2) in
          let s_heap, s_node = (size h, size l1) in
          let s = s_node + s_heap + 1 in
          if s_node >= s_heap then T (s, x, l1, h) else T (s, x, h, l1)
        else
          let h = merge (h1, r2) in
          let s_heap, s_node = (size h, size l2) in
          let s = s_node + s_heap + 1 in
          if s_node >= s_heap then T (s, y, l2, h) else T (s, y, h, l2)

  (* Ex. 3.4 (d)

     In a lazy environment, the top-down merge allows portions of the heap to be merged incrementally.
     The merging process can be paused and resumed as needed, meaning only the necessary parts of the
     tree are evaluated when required. This can lead to better memory usage and performance,
     as unnecessary computations can be avoided.

     In a concurent enviroment the top-down approach allows for more fine-grained parallelism
     because each recursive call to merge works on a smaller part of the heap.
     These smaller tasks can potentially be distributed across multiple threads. *)

  let insert (x, h) = merge (T (1, x, E, E), h)
  let delete_min = function E -> None | T (_, _, l, r) -> Some (merge (l, r))
  let find_min = function E -> None | T (_, x, _, _) -> Some x
end
