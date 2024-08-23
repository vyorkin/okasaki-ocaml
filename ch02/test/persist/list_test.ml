open Core
open Support
open Ch02.List

let propLength l = List.(length l + 1 = length @@ suffixes l)
let propHasEmptyListAsSuffix l = isSuffixOf ~eq:Int.equal ~suffix:[] l

let testLength =
  QCheck.Test.make ~count:50 ~name:"len(l) = n => len(suffixes(l)) = n + 1"
    QCheck.(list small_nat)
    propLength

let testEmpty =
  QCheck.Test.make ~count:30 ~name:"[] isSuffixOf suffixes(l)"
    QCheck.(list small_nat)
    propHasEmptyListAsSuffix

let suite = [ testLength; testEmpty ]
let tests = List.map ~f:QCheck_alcotest.to_alcotest suite
