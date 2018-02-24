structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val (a, b) = (nextInt (), nextInt ())
  in
    print (if a mod 3 = 0 orelse b mod 3 = 0 orelse (a + b) mod 3 = 0
           then "Possible\n"
           else "Impossible\n")
  end
