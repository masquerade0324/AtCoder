structure A  = Array
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun swap (ary, i) =
    let
        val tmp = A.sub (ary, i)
    in
        A.update (ary, i, A.sub (ary, i + 1));
        A.update (ary, i + 1, tmp)
    end

val () =
    let
        val N = nextInt ()
        val n = N mod 30
        val ary = A.tabulate (6, fn i => i + 1)
        val i = ref 0
    in
        while !i < n do (
            swap (ary, !i mod 5);
            i := !i + 1
        );
        A.app (fn x => print (I.toString x)) ary;
        print "\n"
    end
