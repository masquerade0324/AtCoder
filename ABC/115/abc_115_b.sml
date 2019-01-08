structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val n = nextInt ()
        val items = L.tabulate (n, fn _ => nextInt ())
        val (sum, max) = foldl (fn (i, (s, m)) => (s + i, I.max (i, m)))
                               (0, 0) items
    in
        print (I.toString (sum - max div 2) ^ "\n")
    end
