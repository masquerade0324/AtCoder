structure C  = Char
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val _  = nextInt ()
      val zs = map (fn c => if c = #"I" then 1 else ~1) (explode (next ()))
      val ruisekiwa =
          foldl (fn (x, sum :: l) => sum + x :: sum :: l | (_, []) => []) [0] zs
    in
      print (Int.toString (foldl (Int.max) 0 ruisekiwa) ^ "\n")
    end
