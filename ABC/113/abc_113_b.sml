structure I  = Int
structure L  = List
structure R  = Real
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextReal () = valOf (T.scanStream R.scan T.stdIn)

fun minIdx ((i, a : real), (j, b : real)) = if a < b then (i, a) else (j, b)

val () =
    let
        val n = nextInt ()
        val (t, a) = (nextReal (), nextReal ())
        val hs = L.tabulate (n, fn i => (i + 1, nextReal ()))
        val diffs = map (fn (i, h) => (i, Real.abs (t - h * 0.006 - a))) hs
        val (idx, _) = foldl minIdx (0, 100000.0) diffs
    in
        print (I.toString idx ^ "\n")
    end
