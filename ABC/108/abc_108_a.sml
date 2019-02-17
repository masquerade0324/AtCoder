structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val K = nextInt ()
    in
        print (I.toString (((K + 1) div 2) * (K div 2)) ^ "\n")
    end
