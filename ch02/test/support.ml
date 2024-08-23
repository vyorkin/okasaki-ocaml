open Core

let rec dropLengthOpt l1 l2 =
  match (l1, l2) with
  | _, [] -> None
  | [], ys -> Some ys
  | _ :: xs, _ :: ys -> dropLengthOpt xs ys

let rec dropLength l1 l2 =
  match (l1, l2) with
  | _, [] -> []
  | [], ys -> ys
  | _ :: xs, _ :: ys -> dropLength xs ys

let isSuffixOf eq s l =
  match s with
  | [] -> true
  | _ -> (
      match dropLengthOpt s l with
      | None -> false
      | Some d ->
          let s' = dropLength d l in
          List.equal eq s s')
