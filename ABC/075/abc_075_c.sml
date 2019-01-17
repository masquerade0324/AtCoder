structure A  = Array
structure A2 = Array2
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val (N, M) = (nextInt (), nextInt ())

val G = A2.array (N, N, false)

val isVisited = A.array (N, false)

val edges = L.tabulate (M, fn _ => (nextInt () - 1, nextInt () - 1))

val _ = L.app (fn (u, v) => (A2.update (G, u, v, true);
                             A2.update (G, v, u, true))) edges

fun dfs u =
    let
        val v = ref 0
    in
        A.update (isVisited, u, true);
        while !v < N do (
            if A2.sub (G, u, !v) andalso not (A.sub (isVisited, !v))
            then 
                dfs (!v)
            else
                ();
            v := !v + 1
        )
    end

fun solve ([], r)         = r
  | solve ((u, v)::es, r) = (A.modify (fn _ => false) isVisited;
                             A2.update (G, u, v, false);
                             A2.update (G, v, u, false);
                             dfs 0;
                             A2.update (G, u, v, true);
                             A2.update (G, v, u, true);
                             solve (es, r + (if A.all (fn b => b) isVisited
                                             then 0 else 1)))

val () =
    let
       val ans = solve (edges, 0) 
    in
        print (I.toString ans ^ "\n")
    end
