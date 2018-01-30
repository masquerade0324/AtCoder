structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun toStr i = if i >= 0 then Int.toString i else "-" ^ Int.toString (~i)

val () =
  let
    val r = nextInt ()
    val g = nextInt ()
  in
    (* g = (r + b) / 2 -> b = 2 * g - r *)
    print (toStr (2 * g - r) ^ "\n")
  end
