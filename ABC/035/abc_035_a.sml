structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (W, H) = (nextInt (), nextInt ())
    in
        if W * 3 = H * 4 then print "4:3\n" else print "16:9\n"
    end
