structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b) = (nextInt (), nextInt ())
    in
      print (Int.toString ((a + b) mod 24) ^ "\n")
    end
