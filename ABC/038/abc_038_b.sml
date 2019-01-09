structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (h1, w1) = (nextInt (), nextInt ())
        val (h2, w2) = (nextInt (), nextInt ())
    in
        if h1 = h2 orelse w1 = h2 orelse h1 = w2 orelse w1 = w2
        then print "YES\n" else print "NO\n"
    end
