structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
      val (d, n) = (nextInt (), nextInt ())
    in
      if d = 0 then
        if n = 100 then print "101\n" else print (I.toString n ^ "\n")
      else if d = 1 then
        if n = 100 then print "10100\n" else print (I.toString (100 * n) ^ "\n")
      else
        if n = 100 then print "1010000\n" else print (I.toString (10000 * n) ^ "\n")
    end
