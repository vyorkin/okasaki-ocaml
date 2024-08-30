open Core
open Okasaki_common

module MkBinominalHeap (E : Ordered.S) : Heap_intf.S with type elem = E.t =
struct
  type elem = E.t [@@deriving eq, show { with_path = false }]

  type t = E | T of int * elem * t * t
  [@@deriving eq, show { with_path = false }]

  let empty = E
  let is_empty = function E -> true | _ -> false
  let insert = failwith "not implemented"
  let merge = failwith "not implemented"
  let find_min = failwith "not implemented"
  let delete_min = failwith "not implemented"
end
