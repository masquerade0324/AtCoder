structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun maxDiffSub (x, [])      = 0
  | maxDiffSub (x, (y::ys)) = I.max (I.abs (x - y), maxDiffSub (x, ys))

fun maxDiff []        = 0
  | maxDiff (x :: xs) = I.max (maxDiffSub (x, xs), maxDiff xs)

val () =
    let
        val n    = nextInt ()
        val list = L.tabulate (n, fn _ => nextInt ())
        val ans  = maxDiff list
    in
        print (I.toString ans ^ "\n")
    end
                
