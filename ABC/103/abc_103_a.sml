structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (A1, A2, A3) = (nextInt (), nextInt (), nextInt ())
    in
        print (I.toString (I.max (I.max (A1, A2), A3) -
                           I.min (I.min (A1, A2), A3)) ^ "\n")
    end
