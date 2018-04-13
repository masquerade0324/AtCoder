structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, c) = (nextInt (), nextInt (), nextInt ())
    in
      if a + b >= c then print "Yes\n" else print "No\n"
    end
