structure SC = StringCvt
structure T  = TextIO

fun nextLInt () = valOf (T.scanStream (LargeInt.scan SC.DEC) T.stdIn)

val () =
    let
      val x = nextLInt ()
      fun minTime x t : LargeInt.int =
          if x <= t * (t + 1) div 2 then t else minTime x (t + 1)
    in
      print (LargeInt.toString (minTime x 0) ^ "\n")
    end
