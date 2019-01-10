structure A  = Array
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO
structure W  = Word

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (n, X) = (nextInt (), W.fromInt (nextInt ()))
        val ary = A.tabulate (n, fn _ => nextInt ())
        val i   = ref 0
        val sum = ref 0
    in
        while !i < n do (
            if W.andb (X, W.<< (0w1, W.fromInt (!i))) <> 0w0
            then sum := !sum + A.sub (ary, !i) else ();
            i := !i + 1
        );
        print (I.toString (!sum) ^ "\n")
    end
