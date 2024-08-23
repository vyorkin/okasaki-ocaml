exception EmptyList

type 'a t = 'a list

let empty = []
let is_empty s = match s with [] -> true | _ -> false
let cons (x, s) = x :: s
let head s = match s with [] -> raise EmptyList | h :: _ -> h
let tail s = match s with [] -> raise EmptyList | _ :: tl -> tl
let rec ( ++ ) xs ys = match xs with [] -> ys | x :: xs' -> x :: (xs' ++ ys)
