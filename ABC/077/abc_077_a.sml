structure T  = TextIO
structure S  = String
structure SC = StringCvt

fun scan rdr strm = SOME (SC.splitl (not o Char.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
  let
    val cs1 = explode (next ())
    val cs2 = explode (next ())
  in
    if cs1 = rev cs2 then print "YES\n" else print "NO\n"
  end
