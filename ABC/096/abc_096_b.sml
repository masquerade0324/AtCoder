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

fun pow a 0 = 1
  | pow a b = a * pow a (b - 1)

val () =
    let
      val (a, b, c) = (nextInt (), nextInt (), nextInt ())
      val k = nextInt ()
      val p = pow 2 k
      val maxSum = Int.max (Int.max (a * p + b + c, b * p + a + c),
                            c * p + a + b)
    in
      print (Int.toString maxSum ^ "\n")
    end
