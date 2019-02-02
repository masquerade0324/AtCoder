structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (N, K) = (nextInt (), nextInt ())
    in
        if N mod K = 0 then print "0\n" else print "1\n"
    end
