structure I  = Int
structure L  = List
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val () =
    let
        val (N, M) = (nextInt (), nextInt ())
        val (X, Y) = (nextLInt (), nextLInt ())
        val xs = L.tabulate (N, fn _ => nextLInt ())
        val ys = L.tabulate (M, fn _ => nextLInt ())

        fun go ([]   , vs, t, n) = n
          | go (u::us, vs, t, n) = if t <= u then back (us, vs, u + X, n)
                                   else go (us, vs, t, n)
        and back (us, []   , t, n) = n
          | back (us, v::vs, t, n) = if t <= v then go (us, vs, v + Y, n + 1)
                                     else back (us, vs, t, n)
    in
        print (I.toString (go (xs, ys, 0, 0)) ^ "\n")
    end
        
