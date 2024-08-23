open Core
open Okasaki_common

(* More of my notes are here:
   https://github.com/vyorkin/okasaki-smlnj/blob/master/ch03/heap.sml *)

module type S = sig
  type t
  type elem

  (* include Gen.S with type t := t *)

  val empty : t
  val is_empty : t -> bool
  val rank : t -> int
  val insert : elem * t -> t
  val merge : t * t -> t
  val findMin : t -> elem option
  val deleteMin : t -> t option
end
