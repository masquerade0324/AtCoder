structure A  = Array
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val cnts = A.array (100001, 0)
        val n = nextInt ()
        val l = L.tabulate (n, fn _ => nextInt ())
    in
        L.app (fn a => A.update (cnts, a, A.sub (cnts, a) + 1)) l;
        print (I.toString (
                    A.foldl (fn (i, s) => s + I.max (i - 1, 0)) 0 cnts) ^ "\n")
    end
