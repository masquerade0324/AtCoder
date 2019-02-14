structure A2 = Array2
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (N, M, Q) = (nextInt (), nextInt (), nextInt ())
        val lrs = L.tabulate (M, fn _ => (nextInt (), nextInt ()))
        val pqs = L.tabulate (Q, fn _ => (nextInt (), nextInt ()))
        val ary = A2.array (N + 1, N + 1, 0)
        val sum = A2.array (N + 1, N + 1, 0)
        val (i, j) = (ref 1, ref 1)
    in
        L.app (fn (l, r) => A2.update (ary, l, r, A2.sub (ary, l, r) + 1)) lrs;
        i := 1;
        while !i <= N do (
            j := 1;
            while !j <= N do (
                A2.update (sum, !i, !j, A2.sub (sum, !i, !j - 1) +
                                        A2.sub (ary, !i, !j));
                j := !j + 1
            );
            i := !i + 1
        );
        j := 1;
        while !j <= N do (
            i := 1;
            while !i <= N do (
                A2.update (sum, !i, !j, A2.sub (sum, !i - 1, !j) +
                                        A2.sub (sum, !i, !j));
                i := !i + 1
            );
            j := !j + 1
        );
        L.app (fn (p, q) =>
                  print (I.toString (A2.sub (sum, q, q) -
                                     A2.sub (sum, p - 1, q) -
                                     A2.sub (sum, q, p - 1) +
                                     A2.sub (sum, p - 1, p - 1)) ^ "\n")) pqs
    end
