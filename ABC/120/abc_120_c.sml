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
        val s = next ()
        val cnt0 = CV.foldl (fn (c, i) => if c = #"0" then i + 1 else i) 0 s
        val cnt1 = CV.foldl (fn (c, i) => if c = #"1" then i + 1 else i) 0 s
    in
        print (I.toString (I.min (cnt0, cnt1) * 2) ^ "\n")
    end
