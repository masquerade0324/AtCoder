structure A  = Array
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun update (ary, l, r, t) =
    let
        val i = ref l
    in
        while !i <= r do (
            A.update (ary, !i, t);
            i := !i + 1
        )
    end

val () =
    let
        val (N, Q) = (nextInt (), nextInt ())
        val ary = A.array (N, 0)
        val i = ref 0

    in
        while !i < Q do (
            update (ary, nextInt () - 1, nextInt () - 1, nextInt ());
            i := !i + 1
        );
        A.app (fn a => print (I.toString a ^ "\n")) ary
    end
