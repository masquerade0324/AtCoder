structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
      val _ = nextInt ()
      val m = nextInt ()
      val x = nextInt ()
      val l = L.tabulate (m, fn _ => nextInt ())
      val d = length (L.filter (fn i => i < x) l)
    in
      print (I.toString (I.min (d, m - d)) ^ "\n")
    end
