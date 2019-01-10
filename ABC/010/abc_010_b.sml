structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun mushiru 0 = 0
  | mushiru n = if n mod 2 = 0 orelse n mod 3 = 2 then 1 + mushiru (n - 1) else 0

val () =
    let
        val n = nextInt ()
        val l = L.tabulate (n, fn _ => nextInt ())
        val ans = foldl (fn (a, sum) => sum + mushiru a) 0 l
    in
        print (I.toString ans ^ "\n")
    end
