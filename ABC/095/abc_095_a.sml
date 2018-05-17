structure C  = Char
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
      val cs  = explode (next ())
      val sum = foldl (op +) 0 (map (fn c => if c = #"o" then 100 else 0) cs)
    in
      print (I.toString (700 + sum) ^ "\n")
    end
