structure C  = Char
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
        val (a, b) = (next (), next ())
    in
        print (I.toString (valOf (Int.fromString (a ^ b)) * 2) ^ "\n")
    end
