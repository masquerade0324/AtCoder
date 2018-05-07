structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, c) = (nextInt (), nextInt (), nextInt ())
    in
      if a + b = c orelse b + c = a orelse c + a = b
      then print "Yes\n" else print "No\n"
    end
