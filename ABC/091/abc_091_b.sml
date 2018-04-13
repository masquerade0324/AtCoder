structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun updateR (x, [])        = [(x, 1)]
  | updateR (x, (y, v)::l) = if x = y then (y, v + 1)::l else (y, v)::updateR (x, l)

fun updateB (x, [])        = [(x, ~1)]
  | updateB (x, (y, v)::l) = if x = y then (y, v - 1)::l else (y, v)::updateB (x, l)

val () =
    let
      val n  = nextInt ()
      val ss = List.tabulate (n, fn _ => next ())
      val m  = nextInt ()
      val ts = List.tabulate (m, fn _ => next ())
      val l  = foldl updateB (foldl updateR [] ss) ts
    in
      print (Int.toString (foldl (fn ((_, v), r) => Int.max (v, r)) 0 l) ^ "\n")
    end
