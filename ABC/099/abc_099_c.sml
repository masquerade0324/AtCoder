structure A  = Array
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure LI = LargeInt
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val xs = [1]
val ys = [46656, 7776, 1296, 216, 36, 6]
val zs = [59049, 6561, 729, 81, 9]

val dp  = A.array (100001, ~1)

val _ = L.app (fn i => A.update (dp, i, 1)) (xs @ ys @ zs)

fun min (a, b, c) = I.min (I.min (a, b), c)

fun get n []     = NONE
  | get n (h::t) = if n >= h then SOME h else get n t

fun f n =
    if A.sub (dp, n) <> ~ 1 then A.sub (dp, n)
    else
      let
        val a = case get n xs of
                    SOME x => f (n - x)
                  | NONE   => 1000000
        val b = case get n ys of
                    SOME y => f (n - y)
                  | NONE   => 1000000
        val c = case get n zs of
                    SOME z => f (n - z)
                  | NONE   => 1000000
        val res = min (a, b, c)
      in
        A.update (dp, n, res + 1);
        res + 1
      end

val () =
    let
      val n = nextInt ()
    in
      print (I.toString (f n) ^ "\n")
    end
