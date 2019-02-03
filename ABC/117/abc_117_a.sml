structure A  = Array
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure LI = LargeInt
structure R  = Real
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextReal () = valOf (T.scanStream R.scan T.stdIn)

val () =
    let
        val (T, X) = (nextReal (), nextReal ())
    in
        print (R.toString (T / X) ^ "\n")
    end
