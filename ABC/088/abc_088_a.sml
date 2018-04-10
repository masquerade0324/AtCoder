structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val n = nextInt ()
      val a = nextInt ()
    in
      if n mod 500 <= a then print "Yes\n" else print "No\n"
    end
