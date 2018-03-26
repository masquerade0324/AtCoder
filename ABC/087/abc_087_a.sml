structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val x' = nextInt () - nextInt ()
      val b  = nextInt ()
    in
      print (Int.toString (x' - x' div b * b) ^ "\n")
    end
