structure I  = Int
structure L  = List
structure M  = Math
structure R  = Real
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun f (x1, y1) (x2, y2) =
    M.sqrt (R.fromInt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)))

val () =
    let
        val s = (nextInt (), nextInt ())
        val g = (nextInt (), nextInt ())
        val (T, V)   = (nextInt (), nextInt ())
        val n = nextInt ()
        val l = L.tabulate (n, fn _ => (nextInt (), nextInt ()))
        val d = R.fromInt (T * V)
    in
        if L.exists (fn p => f s p + f g p <= d) l
        then print "YES\n" else print "NO\n"
    end
