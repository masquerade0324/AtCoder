structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (a, b) = (nextInt (), nextInt ())
        val d = I.abs (a - b)
    in
        print (I.toString (I.min (d, 10 - d)) ^ "\n")
    end
