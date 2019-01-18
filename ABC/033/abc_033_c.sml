structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val S = next ()
        val ss = S.tokens (fn c => c = #"+") S
        val ans = foldl (fn (s, n) => if CV.exists (fn c => c = #"0") s
                                      then n else n + 1) 0 ss
    in
        print (I.toString ans ^ "\n")
    end
