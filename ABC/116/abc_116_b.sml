structure A  = Array
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure LI = LargeInt
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun collatz n = if n mod 2 = 0 then n div 2 else 3 * n + 1

fun solve (n, r) = if n = 4 then r + 3 else solve (collatz n, r + 1)

val () =
    let
        val s = nextInt ()
    in
        if s = 1 orelse s = 2 orelse s =4 then print "4\n"
        else print (I.toString (solve (s, 1)) ^ "\n")
    end
