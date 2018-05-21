structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, c, d) = (nextInt (), nextInt (), nextInt (), nextInt ())
    in
      print (Int.toString (Int.max (a * b, c * d)) ^ "\n")
    end
