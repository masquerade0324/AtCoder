structure T  = TextIO
structure SC = StringCvt

fun scan rdr strm =
  SOME (SC.splitl (not o Char.isSpace) rdr (SC.skipWS rdr strm))
 
fun next () = valOf (T.scanStream scan T.stdIn)

fun merge _ ([], ys) = ys 
  | merge _ (xs, []) = xs
  | merge gt (x::xs, y::ys) = if gt (x, y) then x::merge gt (xs, y::ys)
                              else y::merge gt (x::xs, ys)

fun split ([], xs, ys)      = (xs, ys)
  | split ([x], xs, ys)     = (x::xs, ys)
  | split (x::y::l, xs, ys) = split (l, x::xs, y::ys)

fun msort _ []  = []
  | msort _ [x] = [x]
  | msort gt xs  =
  let
    val (left, right) = split (xs, [], [])
  in
    merge gt (msort gt left, msort gt right)
  end

val () =
  let
    val s = next ()
    val t = next ()
  in
    if implode (msort (op <) (explode s)) < implode (msort (op >) (explode t))
    then print "Yes\n" else print "No\n"
  end
