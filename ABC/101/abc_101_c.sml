structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val K = nextInt ()
        val l = L.tabulate (N, fn _ => nextInt ())
        val ans = if N <= K then 1 else (N - K - 1) div (K - 1) + 2
    in
        print (I.toString ans ^ "\n")
    end
