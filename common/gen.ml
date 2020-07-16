open Core_kernel

module type S = sig
  type t

  val pp : Format.formatter -> t -> unit

  val gen : t QCheck.Gen.t

  val arbitrary : t QCheck.arbitrary

  val generate : int -> t list

  val generate1 : unit -> t
end
