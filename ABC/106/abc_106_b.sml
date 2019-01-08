structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun isOdd n = n mod 2 = 1

fun cntDivisor n =
    let
        val odds = L.filter isOdd (L.tabulate (n, fn i => i + 1))
    in
        foldl (fn (i, x) => if n mod i = 0 then x + 1 else x) 0 odds 
    end

val () =
    let
        val n    = nextInt ()
        val cnts = map cntDivisor (L.tabulate (n, fn i => i + 1))
    in
        print (I.toString (length (L.filter (fn cnt => cnt = 8) cnts)) ^ "\n")
    end
