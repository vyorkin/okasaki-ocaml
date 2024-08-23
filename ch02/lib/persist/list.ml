let rec ( ++ ) xs ys = match xs with [] -> ys | x :: xs' -> x :: (xs' ++ ys)
let rec suffixes = function [] -> [ [] ] | _ :: t as l -> l :: suffixes t
