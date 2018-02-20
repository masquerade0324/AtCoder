structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () = print (Int.toString ((nextInt () - 1) * (nextInt () - 1)) ^ "\n")
