structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
      val (a, b) = (next (), next ())
    in
      if a = "H" then print (b ^ "\n")
      else
        if b = "H" then print "D\n" else print "H\n"
    end
