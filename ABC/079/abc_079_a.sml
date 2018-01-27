structure T  = TextIO
structure S  = String
structure SC = StringCvt

fun scan rdr strm = SOME (SC.splitl (not o Char.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun judge (s, i) =
  S.sub (s, i) = S.sub (s, i + 1) andalso S.sub (s, i + 1) = S.sub (s, i + 2)

val () =
  let
    val str = next ()
  in
   if judge (str, 0) orelse judge (str, 1) then print "Yes\n" else print "No\n"
  end
