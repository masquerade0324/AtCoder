structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (A, B, C) = (nextInt (), nextInt (), nextInt ())
    in
        print (I.toString (C div I.min (A, B)) ^ "\n")
    end
