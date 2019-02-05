structure A2 = Array2
structure I  = Int
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun fib 1 = 1 : LI.int
  | fib 2 = 1
  | fib n = fib (n - 1) + fib (n - 2)

val M = 1000000007 : LI.int

val (H, W, K) = (nextInt (), nextInt (), nextInt ())

val dp = A2.array (H + 1, W, 0 : LI.int)

val () =
    let
        val h = ref 0
        val w = ref 0
    in
        A2.update (dp, 0, 0, 1);
        while !h < H do (
            w := 0;
            while !w < W do (
                if !w > 0
                then (A2.update (dp, !h + 1, !w - 1,
                                 A2.sub (dp, !h + 1, !w - 1) +
                                 A2.sub (dp, !h, !w) * fib (!w) * fib (W - !w));
                      A2.update (dp, !h + 1, !w - 1, 
                                 A2.sub (dp, !h + 1, !w - 1) mod M))
                else ();
                A2.update (dp, !h + 1, !w,
                           A2.sub (dp, !h + 1, !w) +
                           A2.sub (dp, !h, !w) * fib (!w + 1) * fib (W - !w));
                A2.update (dp, !h + 1, !w,
                           A2.sub (dp, !h + 1, !w) mod M);
                if !w < W - 1
                then (A2.update (dp, !h + 1, !w + 1,
                                 A2.sub (dp, !h + 1, !w + 1) +
                                 A2.sub (dp, !h, !w) * fib (!w + 1) * fib (W - !w - 1));
                      A2.update (dp, !h + 1, !w + 1, 
                                 A2.sub (dp, !h + 1, !w + 1) mod M))
                else ();
                w := !w + 1
            );
            h := !h + 1
        );
        print (LI.toString (A2.sub (dp, H, K - 1)) ^ "\n")
    end
