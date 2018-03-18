structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun solve (xs, []) = xs
  | solve ([], ys) = ys
  | solve (x :: xs, y :: ys) = x :: y :: solve (xs, ys)

val () =
    let
      val (odds, evens) = (explode (next ()), explode (next ()))
    in
      print (implode (solve (odds, evens)) ^ "\n")
    end
