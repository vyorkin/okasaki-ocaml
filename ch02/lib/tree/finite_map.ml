open Core
open Okasaki_common
open Unbalanced_set

module type FiniteMap = sig
  type k
  type v
  type t

  include Gen.S with type t := t

  exception DuplicateKeyError
  exception NotFoundError

  val node : t -> k * v -> t -> t
  val empty : t
  val bind : k * v * t -> t
  val lookup : k * t -> v
end

module MkFiniteMap (K : Ordered.S) (V : Ordered.S) :
  FiniteMap with type k = K.t and type v = V.t = struct
  type k = K.t [@@deriving eq, show { with_path = false }]
  type v = V.t [@@deriving eq, show { with_path = false }]
  type elem = k * v [@@deriving eq, show { with_path = false }]
  type t = E | T of t * elem * t [@@deriving eq, show { with_path = false }]

  let empty = E
  let node l e r = T (l, e, r)

  exception DuplicateKeyError
  exception NotFoundError

  let bind (k, v, t) =
    let e : K.t * V.t = (k, v) in
    let rec insert_node (t, k') =
      let open K in
      if k' = k then raise DuplicateKeyError
      else
        match t with
        | E -> T (E, e, E)
        | T (l, ((k', _) as e'), r) when k <= k' ->
            T (insert_node (l, k'), e', r)
        | T (l, ((k', _) as e'), r) -> T (l, e', insert_node (r, k'))
    in
    try insert_node (t, K.z) with DuplicateKeyError -> t

  let lookup (k, t) =
    let rec find (t, (k', v)) =
      let open K in
      match t with
      | E -> if k = k' then v else raise NotFoundError
      | T (l, ((k', _) as e), _) when k <= k' -> find (l, e)
      | T (_, e, r) -> find (r, e)
    in
    find (t, (K.z, V.z))

  let gen =
    let open QCheck.Gen in
    let aux self = function
      | 0 -> pure empty
      | n ->
          frequency
            [
              (1, pure empty);
              ( 2,
                map3 node
                  (self (n / 2))
                  (QCheck.Gen.pair K.gen V.gen)
                  (self (n / 2)) );
            ]
    in
    sized @@ fix aux

  let arbitrary = QCheck.make gen ~print:show
  let generate n = QCheck.Gen.generate ~n gen
  let generate1 () = QCheck.Gen.generate1 gen
end
