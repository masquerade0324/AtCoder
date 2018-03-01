structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val (_, g, b) = (nextInt (), nextInt (), nextInt ())
  in
    print (if (10 * g + b) mod 4 = 0 then "YES\n" else "NO\n")
  end
