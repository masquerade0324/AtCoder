structure A  = Array
structure I  = Int
structure L  = List
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun ruiseki sums n : LI.int =
    let
      val i = ref 1
    in
      while !i <= n do (
        A.update (sums, !i, A.sub (sums, !i - 1) + nextLInt ());
        i := !i + 1
      );
      A.sub (sums, n)
    end

val () =
    let
      val n     = nextInt ()
      val sums  = A.array (n + 1, LI.fromInt 0)
      val sum   = ruiseki sums n
      val diffs = L.tabulate
                    (n - 1, fn i => LI.abs (A.sub (sums, i + 1) * 2 - sum))
    in
      print (LI.toString (foldl LI.min (hd diffs) (tl diffs)) ^ "\n")
    end
