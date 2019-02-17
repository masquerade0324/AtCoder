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

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun gcd (a, b) = if b = 0 then a else gcd (b, a mod b)

val () =
    let
        val N = nextInt ()
        val l = L.tabulate (N, fn _ => nextInt ())
    in
        print (I.toString (foldl (fn (i, j) => gcd (i, j)) (hd l) l) ^ "\n")
    end
