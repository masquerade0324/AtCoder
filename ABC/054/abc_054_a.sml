structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b) = (nextInt (), nextInt ())
    in
      if a = b then print "Draw\n"
      else if a = 1 then print "Alice\n"
      else if b = 1 then print "Bob\n"
      else if a > b then print "Alice\n"
      else print "Bob\n"
    end
