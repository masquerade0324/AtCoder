structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val (a, b) = (nextInt (), nextInt ())
    val sum    = a + b
  in
    print (if sum >= 10 then "error\n" else Int.toString sum ^ "\n")
  end
