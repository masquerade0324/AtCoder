structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val rate = nextInt ()
    in
        if rate < 1200 then print "ABC\n"
        else if rate < 2800 then print "ARC\n"
        else print "AGC\n"
    end
