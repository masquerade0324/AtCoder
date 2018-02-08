structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val x = nextInt ()
    val t = nextInt ()
  in
    print (Int.toString (Int.max (x - t, 0)) ^ "\n")
  end
