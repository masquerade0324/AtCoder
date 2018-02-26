structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val (a, b, c) = (nextInt (), nextInt (), nextInt ())
    val sum1 = a + b
    val sum2 = a + c
    val sum3 = b + c
  in
    print (Int.toString (Int.min (Int.min (sum1, sum2), sum3)) ^ "\n")
  end
