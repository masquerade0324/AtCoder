structure C  = Char
structure I  = Int
structure L  = List
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
        val ss = L.tabulate (12, fn _ => next ())
    in
        print (I.toString (length (L.filter (S.isSubstring "r") ss)) ^ "\n")
    end
