structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val n = nextInt ()
        val ans = foldl Int.min 1000 (L.tabulate (n, fn _ => nextInt ()))
    in
        print (I.toString ans ^ "\n")
    end

