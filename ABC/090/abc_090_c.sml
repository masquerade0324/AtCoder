structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val () =
    let
      val (n, m) = (nextInt (), nextInt ())
      val answer =
          if n = 1 andalso m = 1 then 1
          else if n = 1 orelse m = 1 then n * m - 2
          else n * m - 2 * (n + m) + 4
    in
      print (LI.toString answer ^ "\n")
    end
