structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (A, B) = (nextInt (), nextInt ())
    in
        print (I.toString ((B + A - 1) div A) ^ "\n")
    end
