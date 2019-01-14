structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun cnt (i, r) = if i mod 2 = 0 then cnt (i div 2, r + 1) else r

val () =
    let
        val N = nextInt ()
        val l = L.tabulate (N, fn _ => nextInt ())
    in
        print (I.toString (foldl (fn (a, s) => s + cnt (a, 0)) 0 l) ^ "\n")
    end
