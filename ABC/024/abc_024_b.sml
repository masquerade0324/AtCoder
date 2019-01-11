structure A  = Array
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (N, T) = (nextInt (), nextInt ())
        val ary = A.array (2000000, 0)
        val l = L.tabulate (N, fn _ => nextInt ())
        val i = ref 1
    in
        L.app (fn a => (A.update (ary, a, A.sub (ary, a) + 1);
                        A.update (ary, a + T, A.sub (ary, a + T) - 1))) l;
        while !i < 2000000 do (
            A.update (ary, !i, A.sub (ary, !i) + A.sub (ary, !i - 1));
            i := !i + 1
        );
        print (I.toString (
                    A.foldl (fn (m, cnt) => if m > 0 then cnt + 1 else cnt) 0 ary
                ) ^ "\n")
    end
