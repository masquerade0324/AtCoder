structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val x = nextInt ()
    in
      print (if x < 1200 then "ABC\n" else "ARC\n")
    end
