structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, h) = (nextInt (), nextInt (), nextInt ())
    in
      print (Int.toString ((a + b) * h div 2) ^ "\n")
    end
