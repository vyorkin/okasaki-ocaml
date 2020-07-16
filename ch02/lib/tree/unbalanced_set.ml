open Core_kernel
open Okasaki_common

module type Set = sig
  type t
  type elem

  include Gen.S with type t := t

  val empty : t

  val insert : elem * t -> t

  val insert' : elem * t -> t

  val insert'' : elem * t -> t

  val member : elem * t -> bool

  val member' : elem * t -> bool

  val complete : elem * int -> t

  val complete' : elem * int -> t
end

module type FiniteMap = sig
  type k
  type v
  type t

  include Gen.S with type t := t

  val empty  : t
  val bind   : k * v * t -> t
  val lookup : k * t -> v option
end

module MkOrderedPair (K : Ordered.S) (V : Ordered.S) = struct
  type t = K.t * V.t [@@deriving show { with_path = false }]

  let name = sprintf "%s * %s" K.name V.name

  let z = (K.z, V.z)

  let mk k v : t = (k, v)

  let ( = ) (k1, _) (k2, _) = K.(k1 = k2)
  let ( < ) (k1, _) (k2, _) = K.(k1 < k2)
  let ( <= ) (k1, _) (k2, _) = K.(k1 <= k2)

  let gen = QCheck.Gen.pair K.gen V.gen

  let arbitrary = QCheck.make gen

  let generate n = QCheck.Gen.generate ~n gen

  let generate1 () = QCheck.Gen.generate1 gen
end

module MkUnbalancedSet (E : Ordered.S) : Set = struct
  type elem = E.t [@@deriving show { with_path = false }]

  type t = E | T of t * elem * t [@@deriving show { with_path = false }]

  let empty = E

  let node l x r = T (l, x, r)

  let rec insert (x, t) =
    let open E in
    match t with
    | E -> T (E, x, E)
    | T (l, y, r) when x < y -> T (insert (x, l), y, r)
    | T (l, y, r) when y < x -> T (l, y, insert (x, r))
    | _ -> t

  exception SameValue

  (* Ex. 2.3 *)
  let insert' (x, t) =
    let open E in
    let rec ins = function
      | E -> T (E, x, E)
      | T (l, v, r) when v < x -> T (ins l, v, r)
      | T (l, v, r) when x < v -> T (l, v, ins r)
      | _ -> raise SameValue
    in try ins t with SameValue -> t

  (* Ex. 2.3 (CPS) *)
  let insert'' (x, t) =
    let open E in
    let rec ins y cont = function
      | E -> insLeaf y cont
      | T (l, z, r) -> insNode y cont (l, z, r)
    and insNode y cont (l, z, r) =
      if x < y
      then ins y (fun l' -> cont (T (l', z, r))) l
      else ins z (fun r' -> cont (T (l, z, r'))) r
    and insLeaf y cont =
      if x = y then t else cont (T (E, x, E))
    in match t with
       | E -> T (E, x, E)
       | T (_, v, _) -> ins v Fn.id t

  let gen =
    let open QCheck.Gen in
    let aux self = function
      | 0 -> pure empty
      | n ->
          frequency
            [(1, pure empty);
             (2, map3 node (self (n / 2)) E.gen (self (n / 2)))]
    in sized @@ fix aux

  let arbitrary = QCheck.make gen ~print:show

  (* show prints smth like:
   * (T ((T ((T (E, 5, E)), 3, E)), 409, (T ((T (E, 9, E)), 6, (T (E, 0, E)))))) *)

  let generate n = QCheck.Gen.generate ~n gen

  let generate1 () = QCheck.Gen.generate1 gen

  let rec member (x, t) =
    let open E in
    match t with
    | E -> false
    | T (l, y, _) when x < y -> member (x, l)
    | T (_, y, r) when y < x -> member (x, r)
    | _ -> true

  let rec mem (x, t, e) =
    let open E in
    match t with
    | E -> x = e
    | T (l, y, _) when x <= y -> mem (x, l, y)
    | T (_, y, r) -> mem (x, r, y)

  (* Ex. 2.2 *)
  let member' (x, t) = mem (x, t, E.z)

  (* Ex. 2.2 (a) *)
  let rec complete (x, d) =
    match d with
    | 0 -> T (E, x, T (E, x, E))
    | 1 -> T (T (E, x, E), x, complete (x, 0))
    | m -> T (complete (x, m - 2), x, complete (x, m - 1))

  (* Ex. 2.5 (b) *)
  let rec complete' (x, d) =
    match d with
    | 0 -> failwith "Zero depth given"
    | 1 -> T (E, x, E)
    | m -> T (complete' (x, m - 1), x, E)

  let%bench_module ("UnbalancedSet"[@name_suffix sprintf "_%s" E.name]) =
    (module struct
      let%bench "member" =
        member (E.generate1 (), generate1 ())

      let%bench "member' (Ex. 2.2)" =
        member' (E.generate1 (), generate1 ())

      let%bench "insert" =
        insert (E.generate1 (), generate1 ())

      let%bench "insert' (Ex. 2.3)" =
        insert' (E.generate1 (), generate1 ())

      let%bench "insert'' (Ex. 2.3 CPS)" =
        insert'' (E.generate1 (), generate1 ())

      let%bench "complete (Ex. 2.6 (a))" =
        complete (E.generate1 (), 20)

      let%bench "complete (Ex. 2.6 (b))" =
        complete' (E.generate1 (), 20)
    end)
end

(* module MkFiniteMap (K : Ordered) (V : Ordered) : FiniteMap = struct
 *   module E = MkOrderedPair (K)(V)
 *   module S = MkUnbalancedSet (E)
 *
 *   type k = K.t
 *   type v = V.t
 *   type t = S.t
 *
 *   let empty = S.empty
 *
 *   let bind (k, v, s) =
 *     let e = E.mk k v in
 *     S.insert (e ??, s)
 * end *)

module IntUS = MkUnbalancedSet (Ordered.Int)
