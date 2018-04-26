structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (n, k, x, y) = (nextInt (), nextInt (), nextInt (), nextInt ())
      val sum = if n <= k then x * n else x * k + (n - k) * y
    in
      print (Int.toString sum ^ "\n")
    end
