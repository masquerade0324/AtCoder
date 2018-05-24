structure C  = Char
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
      val (_, s, _) = (next (), next (), next ())
    in
      print ("A" ^ S.substring (s, 0, 1) ^ "C\n")
    end
