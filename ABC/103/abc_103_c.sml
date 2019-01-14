structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val l = L.tabulate (N, fn _ => nextInt () - 1)
    in
        print (I.toString (foldl (op +) 0 l) ^ "\n")
    end
