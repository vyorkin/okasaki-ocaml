open Core

let isSuffixOf ~eq ~suffix l =
  let suffix' = List.drop l (List.length l - List.length suffix) in
  List.equal eq suffix suffix'
