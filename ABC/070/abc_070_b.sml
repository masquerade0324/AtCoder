structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
  let
    val (a, b, c, d) = (nextInt (), nextInt (), nextInt (), nextInt ())
  in
    print (I.toString (I.max (I.min (b, d) - I.max (a, c), 0)) ^ "\n")
  end
