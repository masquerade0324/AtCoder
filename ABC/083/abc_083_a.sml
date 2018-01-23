structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val (a, b, c, d) = (nextInt (), nextInt (), nextInt (), nextInt ())
    val (l, r) = (a + b, c + d)
  in
    print (if l > r then "Left\n"
           else if l < r then "Right\n"
           else "Balanced\n")
  end
