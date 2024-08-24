open Okasaki_common

module type S = sig
  type elem
  type t

  include Gen.S with type t := t

  val empty : t
  val insert : elem * t -> t
  val member : elem * t -> bool
end
