structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun oddChars []           = []
  | oddChars [c]          = [c]
  | oddChars (c1::c2::cs) = c1::oddChars cs

val () =
  let
    val s = next ()
  in
    print (implode (oddChars (explode s)) ^ "\n")
  end
