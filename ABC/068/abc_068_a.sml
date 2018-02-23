structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () = print ("ABC" ^ Int.toString (nextInt ()) ^ "\n")
