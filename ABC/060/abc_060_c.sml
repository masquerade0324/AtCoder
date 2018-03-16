structure I  = Int
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt ()  = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)
fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val () =
    let
      val n  = nextInt ()
      val T  = nextLInt ()
      val ts = List.tabulate (n, fn _ => nextLInt ())
      fun solve [] _ sum       = sum
        | solve (t::ts) t' sum = solve ts (t + T) (sum + T - LI.max (t' - t, 0))
    in
      print (LI.toString (solve ts 0 0) ^ "\n")
    end
