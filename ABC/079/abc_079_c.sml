structure T  = TextIO
structure SC = StringCvt

fun scan rdr strm = SOME (SC.splitl (not o Char.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val f = Int.toString

val () =
  let
    val [a, b, c, d] = map (fn c => ord c - ord #"0") (explode (next ()))
  in
    if a+b+c+d=7 then print (f a^"+"^f b^"+"^f c^"+"^f d^"=7\n")
    else if a+b+c-d=7 then print (f a^"+"^f b^"+"^f c^"-"^f d^"=7\n")
    else if a+b-c+d=7 then print (f a^"+"^f b^"-"^f c^"+"^f d^"=7\n")
    else if a+b-c-d=7 then print (f a^"+"^f b^"-"^f c^"-"^f d^"=7\n")
    else if a-b+c+d=7 then print (f a^"-"^f b^"+"^f c^"+"^f d^"=7\n")
    else if a-b+c-d=7 then print (f a^"-"^f b^"+"^f c^"-"^f d^"=7\n")
    else if a-b-c+d=7 then print (f a^"-"^f b^"-"^f c^"+"^f d^"=7\n")
    else if a-b-c-d=7 then print (f a^"-"^f b^"-"^f c^"-"^f d^"=7\n")
    else print "\n"
  end
