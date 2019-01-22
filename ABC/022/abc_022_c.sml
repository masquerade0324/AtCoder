structure A  = Array
structure A2 = Array2
structure I  = Int
structure L  = List
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val INF = 100000000

val () =
    let
        val (N, M) = (nextInt (), nextInt ())
        val uvls = L.tabulate (M, fn _ => (nextInt (), nextInt (), nextInt ()))
        val range = L.tabulate (N, fn i => i + 1)
        val d = A2.array (N + 1, N + 1, INF)
        val adj1 = A.fromList (foldl (fn ((u, v, _), l) =>
                                         if u = 1 then v :: l else l) [] uvls)
        val (i, j, k) = (ref 2, ref 2, ref 2)
        val (s, t) = (ref 0, ref 0)
        val len = A.length adj1
        val res = ref INF
    in
        L.app (fn i => A2.update (d, i, i, 0)) range;
        L.app (fn (u, v, l) => (A2.update (d, u, v, l);
                                A2.update (d, v, u, l))) uvls;
        k := 2;
        while !k <= N do (
            i := 2;
            while !i <= N do (
                j := 2;
                while !j <= N do (
                    let val min = I.min (A2.sub (d, !i, !j),
                                         A2.sub (d, !i, !k) + A2.sub (d, !k, !j))
                    in A2.update (d, !i, !j, min) end;
                    j := !j + 1
                );
                i := !i + 1
            );
            k := !k + 1
        );
        while !s < len do (
            t := !s + 1;
            while !t < len do (
                let
                    val (a, b) = (A.sub (adj1, !s), A.sub (adj1, !t))
                in
                    if A2.sub (d, a, b) = INF then ()
                    else res := I.min (!res,
                                       A2.sub (d, 1, a) +
                                       A2.sub (d, a, b) +
                                       A2.sub (d, b, 1))
                end;
                t := !t + 1
            );
            s := !s + 1
        );
        if !res = INF then print "-1\n" else print (I.toString (!res) ^ "\n")
    end
