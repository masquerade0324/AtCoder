structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val n  = nextInt ()
        val hs = L.tabulate (n, fn i => i + 1)
        val l = map (fn h =>
                        let val w = n div h
                        in I.abs (h - w) + n - h * w end) hs
    in
        print (I.toString (foldl I.min 100001 l) ^ "\n")
    end
