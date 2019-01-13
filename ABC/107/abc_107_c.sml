structure I  = Int
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (N, K) = (nextInt (), nextInt ())
        val xs = V.tabulate (N, fn _ => nextInt ())
        val i = ref 0
        val ans = ref 1000000000
    in
        while !i + K - 1 < N do (
            ans := I.min (!ans,
                          I.min (I.abs (V.sub (xs, !i)) +
                                 I.abs (V.sub (xs, !i + K - 1) - V.sub (xs, !i)),
                                 I.abs (V.sub (xs, !i + K - 1)) +
                                 I.abs (V.sub (xs, !i) - V.sub (xs, !i + K - 1))));
            i := !i + 1
        );
        print (I.toString (!ans) ^ "\n")
    end
