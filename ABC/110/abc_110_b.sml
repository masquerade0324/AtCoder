structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (N, M, X, Y) = (nextInt (), nextInt (), nextInt (), nextInt ())
        val xs = X :: L.tabulate (N, fn _ => nextInt ())
        val ys = Y :: L.tabulate (M, fn _ => nextInt ())
    in
        if foldl I.max ~200 xs < foldl I.min 200 ys then print "No War\n"
        else print "War\n"
    end
