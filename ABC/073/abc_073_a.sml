structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val n = nextInt ()
  in
    print (if n div 10 = 9 orelse n mod 10 = 9 then "Yes\n" else "No\n")
  end
