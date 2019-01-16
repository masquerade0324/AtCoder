structure A  = Array
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val ary = A.array (N + 2, 0)
        val costs = A.array (N + 2, 0)
        val i = ref 0
        val sum = ref 0
    in
        i := 1;
        while !i <= N do (
            A.update (ary, !i, nextInt ());
            i := !i + 1
        );
        i := 1;
        while !i < N + 2 do (
            sum := !sum + I.abs (A.sub (ary, !i) - A.sub (ary, !i - 1));
            i := !i + 1
        );
        i := 1;
        while !i <= N do (
            A.update (costs,
                      !i,
                      !sum - I.abs (A.sub (ary, !i) - A.sub (ary, !i - 1)) -
                      I.abs (A.sub (ary, !i + 1) - A.sub (ary, !i)) +
                      I.abs (A.sub (ary, !i + 1) - A.sub (ary, !i - 1)));
            print (I.toString (A.sub (costs, !i)) ^ "\n");
            i := !i + 1
        )
    end
