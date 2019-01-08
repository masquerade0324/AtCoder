structure I  = Int
structure L  = List
structure LP = ListPair
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val T = nextInt ()
        val cts = L.tabulate (N, fn _ => (nextInt (), nextInt ()))
        val ans =
            foldl I.min 1001 (#1 (LP.unzip (L.filter (fn (_, t) => t <= T) cts)))
    in
        if ans >= 1001 then print "TLE\n" else print (I.toString ans ^ "\n")
    end
