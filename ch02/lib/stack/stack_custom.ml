type 'a t = Nil | Cons of 'a * 'a t

let empty = Nil

let isEmpty = function Nil -> true | Cons _ -> false

let cons (x, s) = Cons (x, s)

let head = function Nil -> failwith "Stack is empty" | Cons (h, _) -> h

let tail = function Nil -> failwith "Stack is empty" | Cons (_, t) -> t
