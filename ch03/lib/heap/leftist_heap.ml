open Core_kernel
open Okasaki_common

module Mk (E: Ordered.S) : Heap_intf.S = struct
  type elem = E.t
  type t = E | T of int * elem * t * t

  let empty = E

  let isEmpty = function
    | E -> true
    | _ -> false

  let rank = function
    | E -> 0
    | T (r, _, _, _) -> r

  let mkT (x, l, r) =
    if rank l >= rank r
    then T (rank l + 1, x, l, r)
    else T (rank r + 1, x, r, l)

  let rec merge = function
    | h, E -> h
    | E, h -> h
    | (T (_, x, l1, r1) as h1)
    , (T (_, y, l2, r2) as h2) ->
       if E.(x <= y)
       then mkT (x, l1, merge (r1, h2))
       else mkT (y, l2, merge (h1, r2))

  let insert (x, h) = merge (T (1, x, E, E), h)

  let findMin = function
    | E -> None
    | T (_, x, _, _) -> Some x

  let deleteMin = function
    | E -> None
    | T (_, _, l, r) -> Some (merge (l, r))
end
