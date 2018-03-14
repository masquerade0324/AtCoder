structure C  = Char
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
  let
    val ss = List.tabulate (3, fn _ => next ())
  in
    print (implode (map (fn s => C.toUpper (S.sub (s, 0))) ss) ^ "\n")
  end
