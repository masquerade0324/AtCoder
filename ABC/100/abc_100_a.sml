structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b) = (nextInt (), nextInt ())
    in
      if a <= 8 andalso b <= 8 then print "Yay!\n" else print ":(\n"
    end
