structure A  = Array
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val ary = A.array (2500, [])
        val N = nextInt ()
        val i = ref 9
        val j = ref 9
        val s = ref 0
    in
        i := 9;
        while !i >= 1 do (
            j := 9;
            while !j >= 1 do (
                s := !s + !i * !j;
                A.update (ary, !i * !j, (!i, !j)::A.sub (ary, !i * !j));
                j := !j - 1
            );
            i := !i - 1
        );
        L.app (fn (i, j) => print (I.toString i ^ " x " ^ I.toString j ^ "\n"))
              (A.sub (ary, !s - N))
    end
