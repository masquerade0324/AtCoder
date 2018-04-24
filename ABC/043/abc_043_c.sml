structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val sum = foldl (op +) 0

val () =
    let
      val n   = nextInt ()
      val xs  = List.tabulate (n, fn _ => nextInt ())
      val ave = sum xs div n
    in
      print (
        Int.toString (
          Int.min (sum (map (fn x => (x - ave) * (x - ave)) xs),
                   sum (map (fn x => (x - ave - 1) * (x - ave - 1)) xs))) ^ "\n")
    end
