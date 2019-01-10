structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val n = nextInt ()
        val l = L.tabulate (n, fn _ => nextInt ())
        val (sum, m) = foldl (fn (a, (s, i)) =>
                                 if a = 0 then (s, i) else (s + a, i + 1))
                             (0, 0) l
    in
        print (I.toString ((sum + m - 1) div m) ^ "\n")
    end
