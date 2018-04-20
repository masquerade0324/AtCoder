structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
      val pieces = explode (next ())
      fun count ([], r)  = r
        | count ([x], r) = r + 1
        | count (x :: y :: l, r) =
          if x = y then count (y :: l, r) else count (y :: l, r + 1)
    in
      print (Int.toString (count (pieces, 0) - 1) ^ "\n")
    end
