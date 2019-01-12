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
        val (n, a, b) = (nextInt (), nextInt (), nextInt ())
        val ps = L.tabulate (n, fn _ => nextInt ())
        val (fst, snd, thd) =
            foldl (fn (p, (f, s, t)) =>
                          if p <= a then (f + 1, s, t)
                          else if a < p andalso p <= b then (f, s + 1, t)
                          else (f, s, t + 1)) (0, 0, 0) ps
    in
        print (I.toString (I.min (I.min (fst, snd), thd)) ^ "\n")
    end
