structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val n = nextInt ()
    in
      print (Int.toString (n * 800 - 200 * (n div 15)) ^ "\n")
    end
