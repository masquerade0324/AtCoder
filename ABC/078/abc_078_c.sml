structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun pow (b, p) = if p = 0 then 1 else b * pow (b, p - 1)

val () =
  let
      val (N, M) = (nextInt (), nextInt ())
  in
      print (I.toString ((1900 * M + 100 * (N - M)) * pow (2, M)) ^ "\n")
  end
