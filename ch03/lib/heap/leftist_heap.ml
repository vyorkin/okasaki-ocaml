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

  (* Should have O(n) complexity. *)
  let from_list l = failwith "not implemented"
end
