structure C  = Char
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
        val digits = map (fn c => ord c - ord #"0") (explode (next ()))
        fun f [a, b, c] n       = [I.abs (100 * a + 10 * b + c - n)]
          | f (a::b::c::d::l) n = I.abs (100 * a + 10 * b + c - n)::f (b::c::d::l) n
    in
        print (I.toString (foldl (I.min) 1000000 (f digits 753)) ^ "\n")
    end
