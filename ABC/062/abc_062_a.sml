structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val grps = [[1, 3, 5, 7, 8, 10, 12], [4, 6, 9, 11], [2]]

fun member x l = List.exists (fn y => x = y) l

fun isSameGrp (x, y) =
  member true (map (fn l => member x l andalso member y l) grps)

val () =
  let
    val pair = (nextInt (), nextInt ())
  in
    print (if isSameGrp pair then "Yes\n" else "No\n")
  end
