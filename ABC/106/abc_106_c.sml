structure C  = Char
structure LI = LargeInt 
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun cntCont1 ([], n)    = n
  | cntCont1 (d::ds, n) = if d = #"1" then cntCont1 (ds, n + 1) else n

val () =
    let
        val S = next ()
        val K = nextLInt ()
        val num = cntCont1 (explode S, 0)
    in
        if LI.fromInt num >= K then print "1\n"
        else print (S.substring (S, num, 1) ^ "\n")
    end
