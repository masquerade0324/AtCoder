structure C  = Char
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun printIntLn i = print (if i >= 0 then I.toString i ^ "\n"
                          else "-" ^ I.toString (~1 * i) ^ "\n")

val () =
    let
        val (L, H) = (nextInt (), nextInt ())
        val N = nextInt ()
        val times = L.tabulate (N, fn _ => nextInt ())
        val anss = map (fn t => if t > H then ~1
                                else if t < L then L - t
                                else 0) times
    in
        L.app printIntLn anss
    end
