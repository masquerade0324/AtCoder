structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val () =
    let
      val (n, m) = (nextLInt (), nextLInt ())
    in
      if n > m div 2 then print (LI.toString (m div 2) ^ "\n")
      else print (LI.toString (n + (m - 2 * n) div 4) ^ "\n")
    end
