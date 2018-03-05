structure A  = Array
structure C  = Char
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm)) 
fun next () = valOf (T.scanStream scan T.stdIn)
fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val n = nextInt ()
    val arare = List.tabulate (n, fn _ => next ())
  in
    print (if List.exists (fn s => s = "Y") arare then "Four\n" else "Three\n")
  end
