type 'a t = Nil | Cons of 'a * 'a t

let empty = Nil
let is_empty = function Nil -> true | Cons _ -> false
let cons (x, s) = Cons (x, s)
let head = function Nil -> failwith "Stack is empty" | Cons (h, _) -> h
let tail = function Nil -> failwith "Stack is empty" | Cons (_, t) -> t

let rec ( ++ ) xs ys =
  match xs with Nil -> ys | Cons (x, xs') -> Cons (x, xs' ++ ys)

let rec update (s, i, v) =
  match (s, i) with
  | Nil, _ -> raise @@ Failure "Stack is empty"
  | Cons (_, xs), 0 -> cons (v, xs)
  | Cons (x, xs), _ -> cons (x, update (xs, i - 1, v))
