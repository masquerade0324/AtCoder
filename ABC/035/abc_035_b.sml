structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        fun go (#"R", (x, y)) = (x + 1, y)
          | go (#"L", (x, y)) = (x - 1, y)
          | go (#"U", (x, y)) = (x, y + 1)
          | go (#"D", (x, y)) = (x, y - 1)
          | go (_   , (x, y)) = (x, y)
        fun dist ((x1, y1), (x2, y2)) = I.abs (x1 - x2) + I.abs (y1 - y2)
        val S = next ()
        val T = nextInt ()
        val d = dist (CV.foldl go (0, 0) S, (0, 0))
        val n = length (L.filter (fn c => c = #"?") (explode S))
    in
        if T = 1 then print (I.toString (d + n) ^ "\n")
        else if d >= n then print (I.toString (d - n) ^ "\n")
        else if (n - d) mod 2 = 0 then print "0\n"
        else print "1\n"
    end
