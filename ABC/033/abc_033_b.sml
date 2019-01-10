structure C  = Char
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val l = L.tabulate (N, fn _ => (next (), nextInt ()))
        val sum = foldl (fn ((_, p), t) => t + p) 0 l
        val name = foldl (fn ((s, p), s') => if p <= sum div 2 then s' else s)
                         "atcoder" l
    in
        print (name ^ "\n")
    end
