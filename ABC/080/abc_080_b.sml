structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun sumDgts (x, acc) = if x = 0 then acc else sumDgts (x div 10, acc + x mod 10)

val () =
  let
    val n = nextInt ()
  in
    if n mod sumDgts (n, 0) = 0 then print "Yes\n" else print "No\n"
  end
