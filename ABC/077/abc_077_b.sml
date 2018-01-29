structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun calSq (n, i) = if i * i > n then (i - 1) * (i - 1) else calSq (n, i + 1)

val () =
  let
    val n = nextInt ()
  in
    print (Int.toString (calSq (n, 0)) ^ "\n")
  end
