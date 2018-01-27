structure A  = Array
structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun luca n =
  let
    val ary = A.array (n + 1, Int.toLarge 0)
    val i = ref 2
  in
    A.update (ary, 0, 2);
    A.update (ary, 1, 1);
    while !i <= n do (
        A.update (ary, !i, A.sub (ary, !i - 1) + A.sub (ary, !i - 2));
        i := !i + 1
    );
    A.sub (ary, n)
  end

val () =
  let
    val n = nextInt ()
  in
    print (LargeInt.toString (luca n) ^ "\n")
  end
