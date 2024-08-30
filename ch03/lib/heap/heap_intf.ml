open Okasaki_common

(* More of my notes are here:
   https://github.com/vyorkin/okasaki-smlnj/blob/master/ch03/heap.sml *)

module type S = sig
  type t
  type elem

  val empty : t
  val is_empty : t -> bool
  val insert : elem * t -> t
  val merge : t * t -> t
  val find_min : t -> elem option
  val delete_min : t -> t option
end
