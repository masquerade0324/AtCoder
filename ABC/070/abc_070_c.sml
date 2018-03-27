structure I  = Int
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun gcd (m, n) : LI.int = if n = 0 then m else gcd (n, m mod n)

fun lcm (m, n) : LI.int = m * n div gcd (m, n)

val () =
  let
    val n = nextInt ()
    val l = List.tabulate (n, fn _ => nextLInt ())
  in
    print (LI.toString (foldl lcm 1 l) ^ "\n")
  end
