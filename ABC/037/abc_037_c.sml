structure A  = Array
structure I  = Int
structure LI = LargeInt
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val K = nextInt ()
        val ary = A.array (N + 1, 0)
        val rsk = A.array (N + 1, 0)
        val i   = ref 1
        val sum = ref 0
    in
        while !i <= N do (
            A.update (ary, !i, nextLInt ());
            A.update (rsk, !i, A.sub (rsk, !i - 1) + A.sub (ary, !i));
            i := !i + 1
        );
        i := 1;
        while (!i <= N - K + 1) do (
            sum := !sum + A.sub (rsk, !i + K - 1) - A.sub (rsk, !i - 1);
            i := !i + 1
        );
        print (LI.toString (!sum) ^ "\n")
    end
