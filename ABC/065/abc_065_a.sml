structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val (x, a, b) = (nextInt (), nextInt (), nextInt ())
  in
    print (if b - a <= 0 then "delicious\n"
           else if b - a <= x then "safe\n"
           else "dangerous\n")
  end
