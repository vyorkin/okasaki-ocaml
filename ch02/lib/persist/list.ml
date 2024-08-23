let rec ( ++ ) xs ys = match xs with [] -> ys | x :: xs' -> x :: (xs' ++ ys)

(* Ex. 2.1. *)
let rec suffixes = function [] -> [ [] ] | _ :: t as l -> l :: suffixes t

(*
   Let's show that complexity and space of suffixes(l) is O(n).

   Given l = [1; 2; ...; n],
   we have the following set of reductions:

     1) 1 :: [2; ...; n] -> [1; 2; ...; n] :: suffixes [2; ...; n]
     2) 2 :: [3; ...; n] -> [2; ...; n] :: suffixes [3; ...; n]
      ..........................................................
   n-1) n-1 :: [n] -> [n-1; n] :: suffixes [n]
     n)   n :: []  -> [n] :: suffixes []
   n+1)        []  -> [[]]
*)
