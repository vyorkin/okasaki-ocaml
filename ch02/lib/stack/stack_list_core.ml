open Core

type 'a t = 'a list

let empty = []
let is_empty s = List.is_empty s
let cons (x, s) = x :: s
let head s = List.hd_exn s
let tail s = List.tl_exn s
let ( ++ ) xs ys = xs @ ys

let rec update (s, i, v) =
  match (s, i) with
  | [], _ -> raise @@ Failure "Stack is empty"
  | _ :: xs, 0 -> v :: xs
  | x :: xs, _ -> x :: update (xs, i - 1, v)
