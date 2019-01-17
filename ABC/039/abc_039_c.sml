structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val onkai = "WBWBWWBWBWBWWBWBWWBWBWBWWBWBWWBWBWBW"

val () =
    let
        val s = next ()
    in
        if s = substring (onkai, 0, 20) then print "Do\n"
        else if s = substring (onkai, 2, 20) then print "Re\n"
        else if s = substring (onkai, 4, 20) then print "Mi\n"
        else if s = substring (onkai, 5, 20) then print "Fa\n"
        else if s = substring (onkai, 7, 20) then print "So\n"
        else if s = substring (onkai, 9, 20) then print "La\n"
        else print "Si\n"
    end
