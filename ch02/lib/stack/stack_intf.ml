module type S = sig
  type 'a t

  val empty : 'a t

  val isEmpty : 'a t -> bool

  val cons : 'a * 'a t -> 'a t

  val head : 'a t -> 'a

  val tail : 'a t -> 'a t
end
