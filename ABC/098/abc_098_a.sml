structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun printIntLn i = if i >= 0 then print (I.toString i ^ "\n")
                   else print ("-" ^ I.toString (~i) ^ "\n")

val () =
    let
      val (a, b) = (nextInt (), nextInt ())
    in
      printIntLn (I.max (I.max (a + b, a - b), a * b))
    end
