structure A  = Array
structure CV = CharVector
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun inc (a, i) = A.update (a, i, A.sub (a, i) + 1)
fun dec (a, i) = A.update (a, i, A.sub (a, i) - 1)

val () =
    let
        val (N, Q) = (nextInt (), nextInt ())
        val ary = A.array (N + 1, 0)
        val i = ref 0
    in
        while !i < Q do (
            inc (ary, nextInt () - 1);
            dec (ary, nextInt ());
            i := !i + 1
        );
        i := 1;
        while !i < N do (
            A.update (ary, !i, A.sub (ary, !i) + A.sub (ary, !i - 1));
            i := !i + 1
        );
        print (CV.tabulate (N, fn j => if A.sub (ary, j) mod 2 = 0
                                       then #"0" else #"1"));
        print "\n"
    end
