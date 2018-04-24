structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val n   = nextInt ()
    in
      print (Int.toString (n * (n + 1) div 2) ^ "\n")
    end
