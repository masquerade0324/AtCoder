structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val (a, b, c) = (nextInt (), nextInt (), nextInt ())
  in
    print (if a <= c andalso c <= b then "Yes\n" else "No\n")          
  end
