structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (w, a, b) = (nextInt (), nextInt (), nextInt ())
    in
      if a < b then print (Int.toString (Int.max (0, b - (a + w))) ^ "\n")
      else print (Int.toString (Int.max (0, a  - (b + w))) ^ "\n")
    end
