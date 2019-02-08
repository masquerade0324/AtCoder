structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () = print (I.toString ((nextInt () - 1) * (nextInt () - 1)) ^ "\n")
