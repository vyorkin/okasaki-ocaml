exception EmptyStack

type 'a t = 'a list

let empty = []
let is_empty s = match s with [] -> true | _ -> false
let cons (x, s) = x :: s
let head s = match s with [] -> raise EmptyStack | h :: _ -> h
let tail s = match s with [] -> raise EmptyStack | _ :: tl -> tl
let rec ( ++ ) xs ys = match xs with [] -> ys | x :: xs' -> x :: (xs' ++ ys)

let rec update (s, i, v) =
  match (s, i) with
  | [], _ -> raise EmptyStack
  | _ :: xs, 0 -> v :: xs
  | x :: xs, _ -> x :: update (xs, i - 1, v)
