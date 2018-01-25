structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val (n, a, b) = (nextInt (), nextInt (), nextInt ())
    val minFee = Int.min (a * n, b)
  in
    print (Int.toString minFee ^ "\n")
  end
