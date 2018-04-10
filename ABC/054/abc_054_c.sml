structure A  = Array2
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun makeEdge graph =
    let
      val (u, v) = (nextInt () - 1, nextInt () - 1)
    in
      A.update (graph, u, v, true);
      A.update (graph, v, u, true)
    end

fun dfs graph n vs d u =
    let
      val vs' = V.update (vs, u, true)
      val nodes = List.tabulate (n, fn i => i)
      fun add (v, sum) =
          if A.sub (graph, u, v) andalso not (V.sub (vs', v))
          then sum + dfs graph n vs' (d + 1) v
          else sum
    in
      if d = n then 1
      else foldl add 0 nodes
    end

val () =
    let
      val (n, m) = (nextInt (), nextInt ())
      val graph = A.array (n, n, false)
      val vs    = V.tabulate (n, fn _ => false)
      val i     = ref 0
    in
      while !i < m do (
        makeEdge graph;
        i := !i + 1
      );
      print (Int.toString (dfs graph n vs 1 0) ^ "\n")
    end
