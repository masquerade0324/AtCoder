structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf ( T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val a = nextInt ()
    val b = nextInt ()
    val ave = if (a + b) mod 2 = 0 then (a + b) div 2 else (a + b) div 2 + 1
  in
    print (Int.toString ave ^ "\n")
  end
