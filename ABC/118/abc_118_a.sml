structure A  = Array
structure A2 = Array2
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure LI = LargeInt
structure S  = String
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (A, B) = (nextInt (), nextInt ())
    in
        if B mod A = 0 then print (I.toString (A + B) ^ "\n")
        else print (I.toString (B - A) ^ "\n")
    end
