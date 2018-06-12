structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, c, d) = (nextInt (), nextInt (), nextInt (), nextInt ())
    in
      if I.abs (a - c) <= d orelse I.abs (a - b) <= d andalso I.abs (b - c) <= d
      then print "Yes\n" else print "No\n"
    end
