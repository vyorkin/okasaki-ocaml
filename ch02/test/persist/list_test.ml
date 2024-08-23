open Core
open Support
open Ch02.List

let propLength l = List.(length l + 1 = length @@ suffixes l)
let propEmpty l = isSuffixOf Int.equal [] l

let testLength =
  QCheck.Test.make ~count:50 ~name:"suffix length"
    QCheck.(list small_int)
    propLength

let testEmpty =
  QCheck.Test.make ~count:30 ~name:"suffix empty"
    QCheck.(list small_int)
    propEmpty

let tests = List.map ~f:QCheck_alcotest.to_alcotest [ testLength; testEmpty ]
