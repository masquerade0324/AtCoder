structure I  = Int
structure LI = LargeInt
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun f ([] : LI.int list) : LI.int list = []
  | f [x] = [1]
  | f (x1::x2::xs) =
    let
        val (n::ns) = f (x2::xs)
    in
        if x1 < x2 then (n + 1)::ns else 1::n::ns
    end

val () =
    let
        val N   = nextInt ()
        val xs  = L.tabulate (N, fn _ => nextLInt ())
        val ans = foldl (fn (n, s) => s + n * (n + 1) div 2) 0 (f xs)
    in
        print (LI.toString ans ^ "\n")
    end
