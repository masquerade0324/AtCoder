structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val n = nextInt ()
        val ts = V.tabulate (n, fn _ => nextInt ())
        val m = nextInt ()
        val pxs = L.tabulate (m, fn _ => (nextInt () - 1, nextInt ()))
        val sum = V.foldl (op +) 0 ts
    in
        app (fn (p, x) =>
                print (I.toString (sum - V.sub (ts, p) + x) ^ "\n")) pxs
    end
