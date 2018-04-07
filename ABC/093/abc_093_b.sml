structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, k) = (nextInt (), nextInt (), nextInt ())
    in
      if b - a <= 2 * k - 2
      then L.tabulate (b - a + 1, fn i => print (Int.toString (a + i) ^ "\n"))
      else (L.tabulate (k, fn i => print (Int.toString (a + i) ^ "\n"));
            L.tabulate (k, fn i => print (Int.toString (b - k + 1 + i) ^ "\n")));
      ()
    end
