structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
      val n = nextInt ()
      val x = nextInt ()
      val l = L.tabulate (n, fn _ => nextInt ())
    in
      print
        (I.toString (n + (x -  foldl (op +) 0 l) div foldl I.min 1000 l) ^
         "\n")
    end
