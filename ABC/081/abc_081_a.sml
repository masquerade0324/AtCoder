structure T  = TextIO
structure SC = StringCvt

fun scan rdr strm =
  SOME (SC.splitl (not o Char.isSpace) rdr (SC.skipWS rdr strm))
 
fun next () = valOf (T.scanStream scan T.stdIn)

val () =
  let
    val cs = explode (next ())
    val sum = foldl (fn (c, i) => if c = #"1" then i + 1 else i) 0 cs;
  in
    print (Int.toString sum ^ "\n")
  end
