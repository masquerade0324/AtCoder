structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (n, x) = (nextInt (), nextInt ())
    in
        print (I.toString (I.min (n - x, x - 1)) ^ "\n")
    end
