structure A  = Array
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val x = nextInt ()
        val flgs = A.array (x + 1, false)
        val b = ref 2
        val m = ref 0
        val i = ref x
    in
        A.update (flgs, 1, true);
        while !b <= x do (
            m := !b * !b;
            while !m <= x do (
                A.update (flgs, !m, true);
                m := !m * !b
            );
            b := !b + 1
        );
        while not (A.sub (flgs, !i)) do (
            i := !i - 1
        );
        print (I.toString (!i) ^ "\n")
    end
