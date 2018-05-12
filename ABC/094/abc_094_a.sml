structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, x) = (nextInt (), nextInt (), nextInt ())
    in
      if a <= x andalso x <= a + b then print "YES\n" else print "NO\n"
    end
