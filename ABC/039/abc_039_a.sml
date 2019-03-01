structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (A, B, C) = (nextInt (), nextInt (), nextInt ())
    in
        print (I.toString ((A * B + B * C + C * A) * 2) ^ "\n")
    end
