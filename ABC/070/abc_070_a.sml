structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let val cs = explode (Int.toString (nextInt ()))
  in  print (if cs = rev cs then "Yes\n" else "No\n")
  end
