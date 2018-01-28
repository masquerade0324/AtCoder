structure T  = TextIO
structure SC = StringCvt
fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)
val () =
  let
    val x = nextInt ()
    val y = nextInt ()
    val z = nextInt ()
  in
    print (Int.toString ((x - z) div (y + z)) ^ "\n")
  end
