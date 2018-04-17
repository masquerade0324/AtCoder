structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val () =
    let
      val x = nextLInt ()
      val q = (x - 1) div 11
    in
      print 
        (LI.toString (if x - (q * 11) <= 6 then q * 2 + 1 else q * 2 + 2) ^ "\n")
    end
