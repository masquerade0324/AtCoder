structure I  = Int
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun pr i = if i >= 0 then print (LI.toString i ^ "\n")
           else print ("-" ^ LI.toString (~i) ^ "\n")

fun nextInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, c, k) = (nextInt (), nextInt (), nextInt (), nextInt ())
    in
      pr (if k mod 2 = 0 then a - b else b - a)
    end
