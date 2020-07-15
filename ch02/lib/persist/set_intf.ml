module type S = sig
  type elem

  type t

  val empty : t

  val insert : elem * t -> t

  val member : elem * t -> bool
end
