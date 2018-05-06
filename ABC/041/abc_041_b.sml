structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, c) = (nextLInt (), nextLInt (), nextLInt ())
    in
      print (LI.toString (a * b * c mod 1000000007) ^ "\n")
    end
