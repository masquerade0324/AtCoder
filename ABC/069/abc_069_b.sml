structure C  = Char
structure I  = Int
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
  let
    val s = next ()
    val n = S.size s
  in
    print (
      S.str (S.sub (s, 0)) ^ I.toString (n - 2) ^ S.str (S.sub (s, n - 1)) ^ "\n"
    )
  end
