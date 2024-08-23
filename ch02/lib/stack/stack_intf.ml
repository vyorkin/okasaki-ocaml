module type S = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val cons : 'a * 'a t -> 'a t
  val head : 'a t -> 'a
  val tail : 'a t -> 'a t
  val ( ++ ) : 'a t -> 'a t -> 'a t
  val update : 'a t * int * 'a -> 'a t
end
