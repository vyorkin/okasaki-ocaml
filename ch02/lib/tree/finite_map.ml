open Core
open Okasaki_common
open Unbalanced_set

module MkOrderedPair (K : Ordered.S) (V : Ordered.S) :
  Ordered.S with type t = K.t * V.t = struct
  type t = K.t * V.t [@@deriving show { with_path = false }]

  let name = sprintf "%s * %s" K.name V.name
  let z = (K.z, V.z)
  let equal (k1, _) (k2, _) = K.(equal k1 k2)
  let ( = ) (k1, _) (k2, _) = K.(k1 = k2)
  let ( < ) (k1, _) (k2, _) = K.(k1 < k2)
  let ( > ) (k1, _) (k2, _) = K.(k1 > k2)
  let ( <= ) (k1, _) (k2, _) = K.(k1 <= k2)
  let gen = QCheck.Gen.pair K.gen V.gen
  let arbitrary = QCheck.make gen
  let generate n = QCheck.Gen.generate ~n gen
  let generate1 () = QCheck.Gen.generate1 gen
end

module type FiniteMap = sig
  type k
  type v
  type t

  include Gen.S with type t := t

  val empty : t
  val bind : k * v * t -> t
  val lookup : k * t -> v option
end

module MkFiniteMap (K : Ordered.S) (V : Ordered.S) : FiniteMap = struct
  module E = MkOrderedPair (K) (V)
  module S = MkUnbalancedSet (E)

  type k = K.t
  type v = V.t
  type t = S.t [@@deriving show { with_path = false }]

  let empty = S.empty

  let bind (k, v, s) =
    let e = (k, v) in
    S.insert (e, s)

  let lookup (_k, _s) =
    failwith "Seems impossible to reuse UnbalancedSet to implement FiniteMap"

  let gen = S.gen
  let arbitrary = QCheck.make gen ~print:show
  let generate n = QCheck.Gen.generate ~n gen
  let generate1 () = QCheck.Gen.generate1 gen
end
