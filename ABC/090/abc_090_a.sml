structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
      val [c11, _, _] = explode (next ())
      val [_, c22, _] = explode (next ())
      val [_, _, c33] = explode (next ())
    in
      print (implode [c11, c22, c33, #"\n"])
    end
