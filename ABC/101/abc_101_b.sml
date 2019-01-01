structure C  = Char
structure I  = Int
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val num    = nextInt ()
        val digits = map (fn c => ord c - ord #"0") (explode (I.toString num))
        val sum    = foldl (op +) 0 digits                  
    in
        if num mod sum = 0 then print "Yes\n" else print "No\n"
    end
