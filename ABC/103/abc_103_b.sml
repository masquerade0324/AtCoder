structure C  = Char
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
        val s = next ()
        val t = next ()
    in
        if S.isSubstring t (s ^ s) then print "Yes\n" else print "No\n"
    end
