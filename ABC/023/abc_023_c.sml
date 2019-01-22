structure A  = Array
structure I  = Int
structure L  = List
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1)

val () =
    let
        val (R, C, K) = (nextInt (), nextInt (), nextInt ())
        val N = nextInt ()
        val rcs = L.tabulate (N, fn _ => (nextInt () - 1, nextInt () - 1))
        val sumr = A.array (R, 0)
        val sumc = A.array (C, 0)
        val nr = A.array (N + 1, 0)
        val nc = A.array (N + 1, 0)
        val cnt = ref 0 : LI.int ref
        val i = ref 0
    in
        L.app (fn (r, c) => (inc (sumr, r); inc (sumc, c))) rcs;
        A.app (fn sum => inc (nr, sum)) sumr;
        A.app (fn sum => inc (nc, sum)) sumc;
        while !i <= K do (
            cnt := !cnt + LI.fromInt (A.sub (nr, !i)) *
                          LI.fromInt (A.sub (nc, K - !i));
            i := !i + 1
        );
        L.app (fn (r, c) => if A.sub (sumr, r) + A.sub (sumc, c) = K
                            then cnt := !cnt - 1
                            else if A.sub (sumr, r) + A.sub (sumc, c) = K + 1
                            then cnt := !cnt + 1
                            else ()) rcs;
        print (LI.toString (!cnt) ^ "\n")
    end
