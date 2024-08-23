module type S = sig
  type t

  include Gen.S with type t := t

  val name : string
  val z : t
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
end

module Int = struct
  open Core.Int

  type t = int [@@deriving show]

  let name = "int"
  let z = min_value
  let ( = ) = ( = )
  let ( < ) = ( < )
  let ( <= ) = ( <= )
  let gen = QCheck.Gen.nat
  let arbitrary = QCheck.make gen
  let generate n = QCheck.Gen.generate ~n gen
  let generate1 () = QCheck.Gen.generate1 gen
end
