structure A  = Array
structure A2 = Array2
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (N, M) = (nextInt (), nextInt ())
        val adj = A2.array (N + 1, N + 1, false)
        val adj2 = A2.array (N + 1, N + 1, false)
        val edges = L.tabulate (M, fn _ => (nextInt (), nextInt ()))
        val cnts = A.array (N + 1, 0)
        val (i, j, k) = (ref 1, ref 1, ref 1)
    in
        L.app (fn (i, j) => (A2.update (adj, i, j, true);
                             A2.update (adj, j, i, true))) edges;
        i := 1;
        while !i <= N do (
            j := 1;
            while !j <= N do (
                k := 1;
                while !k <= N do (
                    if A2.sub (adj, !i, !j) andalso A2.sub (adj, !j, !k) andalso
                       !i <> !k andalso not (A2.sub (adj, !i, !k))
                    then A2.update (adj2, !i, !k, true) else ();
                    k := !k + 1
                );
                j := !j + 1
            );
            j := 1;
            while !j <= N do (
                if A2.sub (adj2, !i, !j)
                then A.update (cnts, !i, A.sub (cnts, !i) + 1) else ();
                j := !j + 1
            );
            print (I.toString (A.sub (cnts, !i)) ^ "\n");
            i := !i + 1
        )
    end
