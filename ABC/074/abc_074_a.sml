structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val n = nextInt ()
    val a = nextInt ()
  in
    print (Int.toString (n * n - a) ^ "\n")
  end
