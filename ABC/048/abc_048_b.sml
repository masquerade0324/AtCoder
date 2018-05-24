structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, x) = (nextInt (), nextInt (), nextInt ())
    in
      print (LI.toString (b div x - (a - 1) div x) ^ "\n")
    end
