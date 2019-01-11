structure A  = Array
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val n = nextInt ()
        val cnts = A.array (n + 1, 0)
        val (a, b) = (nextInt (), nextInt ())
        val k = nextInt ()
        val ps = L.tabulate (k, fn _ => nextInt ())
    in
        A.update (cnts, a, A.sub (cnts, a) + 1);
        A.update (cnts, b, A.sub (cnts, b) + 1);
        L.app (fn p => A.update (cnts, p, A.sub (cnts, p) + 1)) ps;
        if A.all (fn i => i <= 1) cnts then print "YES\n" else print "NO\n"
    end
