structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun printInt i = if i >= 0 then print (Int.toString i ^ "\n")
                 else print ("-" ^ Int.toString (~i) ^ "\n")

val () =
  let
    val (a, b, c) = (nextInt (), nextInt (), nextInt ())
  in
    printInt (if a = b then c else if a = c then b else a)
  end
