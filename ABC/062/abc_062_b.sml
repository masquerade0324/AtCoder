structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o Char.isSpace) rdr (SC.skipWS rdr strm))
fun nextStr () = valOf (T.scanStream scan T.stdIn)
fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val h = ref (nextInt ())
    val w = nextInt ()
    val str = implode (List.tabulate (w + 2, fn _ => #"#"))
  in
    print (str ^ "\n");
    while !h > 0 do (
      print ("#" ^ nextStr () ^ "#\n");
      h := !h - 1
    );
    print (str ^ "\n")
  end
