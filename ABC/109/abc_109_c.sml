structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun gcd (a, b) = if b = 0 then a else gcd (b, a mod b)

fun gcdl [m] = m
  | gcdl (m1::m2::ms) = gcdl (gcd (m1, m2)::ms)

val () =
    let
        val (N, X) = (nextInt (), nextInt ())
        val xs = L.tabulate (N, fn _ => I.abs (nextInt () - X))
    in
        print (I.toString (gcdl xs) ^ "\n")
    end
