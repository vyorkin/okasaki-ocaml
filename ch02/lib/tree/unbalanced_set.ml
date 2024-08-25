open Core
open Okasaki_common

module type Set = sig
  type elem
  type t

  include Gen.S with type t := t

  val node : t -> elem -> t -> t
  val equal : t -> t -> bool
  val empty : t
  val insert : elem * t -> t
  val member : elem * t -> bool
  val draw : ?prefix:string -> ?is_left:bool -> t -> unit
  val member_ex_2_2 : elem * t -> bool
  val insert_ex_2_3 : elem * t -> t
  val complete_ex_2_5_a : elem * int -> t
  val complete_ex_2_5_b : elem * int -> t
end

module MkUnbalancedSet (E : Ordered.S) : Set with type elem := E.t = struct
  type elem = E.t [@@deriving eq, show { with_path = false }]
  type t = E | T of t * elem * t [@@deriving eq, show { with_path = false }]

  let empty = E
  let node l x r = T (l, x, r)

  let rec insert (x, t) =
    let open E in
    match t with
    | E -> T (E, x, E)
    | T (l, y, r) when x < y -> T (insert (x, l), y, r)
    | T (l, y, r) when x > y -> T (l, y, insert (x, r))
    | _ -> t

  let gen =
    let open QCheck.Gen in
    let aux self = function
      | 0 -> pure empty
      | n ->
          frequency
            [
              (1, pure empty); (2, map3 node (self (n / 2)) E.gen (self (n / 2)));
            ]
    in
    sized @@ fix aux

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
    | T (_, y, r) when x > y -> member (x, r)
    | _ -> true

  let rec draw ?(prefix = "") ?(is_left = true) tree =
    match tree with
    | E -> Printf.printf "%s%s\n" prefix (if is_left then "└── " else "┌── ")
    | T (left, value, right) ->
        Printf.printf "%s%s%s\n" prefix
          (if is_left then "└── " else "┌── ")
          (show_elem value);
        let new_prefix = prefix ^ if is_left then "    " else "│   " in
        draw ~prefix:new_prefix ~is_left:false right;
        draw ~prefix:new_prefix ~is_left:true left

  let%bench_module ("UnbalancedSet" [@name_suffix sprintf "_%s" E.name]) =
    (module struct
      let%bench "member" = member (E.generate1 (), generate1 ())
      let%bench "insert" = insert (E.generate1 (), generate1 ())
    end)

  (* Ex. 2.2:
     Rewrite member to take no more than d + 1 comparisons by
     keeping track of a candidate element that might be
     equal to the query element (say, the last element for
     which < returned false or ≤ returned true) and
     checking for equality only when you hit the bottom of the tree. *)
  let member_ex_2_2 (x, t) =
    let rec member (x, t, e) =
      let open E in
      match t with
      | E -> x = e (* Reached bottom of the tree *)
      | T (l, y, _) when x <= y -> member (x, l, y)
      | T (_, y, r) -> member (x, r, y)
    in
    (* Assuming tree doesn't have E.z elements. *)
    member (x, t, E.z)

  exception SameValueError

  (* Ex. 2.3 *)
  let insert_ex_2_3 (x, t) =
    let open E in
    let rec insert_node = function
      | E -> T (E, x, E)
      | T (l, y, r) when x < y -> T (insert_node l, y, r)
      | T (l, y, r) when x > y -> T (l, y, insert_node r)
      (* The element being inserted is already present in the tree,
         raise an error to avoiding any unnecessary copying of nodes. *)
      | _ -> raise SameValueError
    in
    (* We already have that element,
       so lets just return the original tree. *)
    try insert_node t with SameValueError -> t

  (* Ex. 2.5 (a) *)
  let rec complete_ex_2_5_a (x, d) =
    match d with
    | 0 -> T (E, x, T (E, x, E))
    | 1 -> T (T (E, x, E), x, complete_ex_2_5_a (x, 0))
    | m -> T (complete_ex_2_5_a (x, m - 2), x, complete_ex_2_5_a (x, m - 1))

  (* Ex. 2.5 (b) *)
  let rec complete_ex_2_5_b (x, d) =
    match d with
    | 0 -> failwith "Zero depth given"
    | 1 -> T (E, x, E)
    | m -> T (complete_ex_2_5_b (x, m - 1), x, E)

  let%bench_module ("UnbalancedSet (exercises)"
      [@name_suffix sprintf "_%s" E.name]) =
    (module struct
      let%bench "member (Ex. 2.2)" = member_ex_2_2 (E.generate1 (), generate1 ())
      let%bench "insert (Ex. 2.3)" = insert_ex_2_3 (E.generate1 (), generate1 ())
      let%bench "complete (Ex. 2.5 (a))" = complete_ex_2_5_a (E.generate1 (), 20)
      let%bench "complete (Ex. 2.5 (b))" = complete_ex_2_5_b (E.generate1 (), 20)
    end)
end
