structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, c) = (nextInt (), nextInt (), nextInt ())
    in
      if a = b andalso b = c then print "1\n"
      else if a = b orelse b = c orelse c = a then print "2\n"
      else print "3\n"
    end
