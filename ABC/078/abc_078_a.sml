structure T  = TextIO
structure SC = StringCvt
fun nextHex () = valOf (T.scanStream (Int.scan SC.HEX) T.stdIn)
val () =
  let
    val x = nextHex ()
    val y = nextHex ()
  in
    print (if x < y then "<\n" else if x > y then ">\n" else "=\n")
  end
