open Core_kernel

type 'a t = 'a list

let empty = []

let isEmpty s = List.is_empty s

let cons (x, s) = x :: s

let head s = List.hd_exn s

let tail s = List.tl_exn s
