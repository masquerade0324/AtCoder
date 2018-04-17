structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () = print (Int.toString (Int.min (nextInt (), nextInt ()) +
                              Int.min (nextInt (), nextInt ())) ^ "\n")
