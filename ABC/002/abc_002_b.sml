structure C  = Char
structure I  = Int
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun conv c = if c = #"a" orelse c = #"i" orelse c = #"u" orelse
                c = #"e" orelse c = #"o"
             then "" else str c

val () = print (S.translate conv (next ()) ^ "\n")
