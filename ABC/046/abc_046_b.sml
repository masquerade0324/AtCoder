structure I  = Int
structure II = IntInf
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)
fun nextIInt () = valOf (T.scanStream (II.scan SC.DEC) T.stdIn)

val () =
    let
      val (n, k) = (nextInt (), nextIInt ())
    in
      print (II.toString (II.pow (k - 1, n - 1) * k) ^ "\n")
    end
