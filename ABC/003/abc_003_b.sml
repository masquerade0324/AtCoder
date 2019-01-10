structure C  = Char
structure I  = Int
structure L  = List
structure LP = ListPair
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun pred (c1, c2) = c1 = c2 orelse
                    c1 = #"@" andalso S.isSubstring (str c2) "atcoder" orelse
                    c2 = #"@" andalso S.isSubstring (str c1) "atcoder"

val () =
    let
        val pl = LP.zip (explode (next ()), explode (next ()))
    in
        if L.all pred pl then print "You can win\n" else print "You will lose\n"
    end
