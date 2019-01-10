structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (A, B, C) = (nextInt (), nextInt (), nextInt ())
    in
        if B = 0 then
            if A = C then print "?\n"
            else print "!\n"
        else
            if A + B = C then print "+\n"
            else if A - B = C then print "-\n"
            else print "!\n"
    end
