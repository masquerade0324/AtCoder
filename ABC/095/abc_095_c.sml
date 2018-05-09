structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, c) = (nextInt (), nextInt (), nextInt ())
      val (x, y)    = (nextInt (), nextInt ())
      val res = if x < y
                then I.min (a + b, 2 * c) * x + I.min (b, 2 * c) * (y - x)
                else I.min (a + b, 2 * c) * y + I.min (a, 2 * c) * (x - y)
    in
      print (I.toString res ^ "\n")
    end
