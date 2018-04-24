structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun edit ([], rs)              = rs
  | edit (#"B" :: cs, [])      = edit (cs, [])
  | edit (#"B" :: cs, r :: rs) = edit (cs, rs)
  | edit (c :: cs, rs)         = edit (cs, c :: rs)

val () =
    let
      val cs = explode (next ())
    in
      print (implode (rev (#"\n" :: edit (cs, []))))
    end
