structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (IntInf.scan SC.DEC) T.stdIn)

val () =
  let
    val a = nextInt ()
    val b = nextInt ()
  in
    if a > b then print "GREATER\n"
    else if a < b then print "LESS\n"
    else print "EQUAL\n"
  end
