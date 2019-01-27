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

val () =
    let
        val (N, A, B) = (nextInt (), nextInt (), nextInt ())
    in
        print (I.toString (I.min (A, B)) ^ " ");
        print (I.toString (I.max (A + B - N, 0)) ^ "\n")
    end
