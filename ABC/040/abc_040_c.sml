structure A  = Array
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val n   = nextInt ()
val ary = A.array (n + 1, 0)
val dp  = A.array (n + 1, ~1)

val min = I.min
val abs = I.abs
val s   = A.sub
val up  = A.update

fun f (n, ary) =
    if s (dp, n) <> ~1 then s (dp, n)
    else
      let
        val res = 
            if n <= 2 then abs (s (ary, 2) - s (ary, 1))
            else if n = 3 then
              min (abs (s (ary, 3) - s (ary, 2)) + abs (s (ary, 2) - s (ary, 1)),
                   abs (s (ary, 3) - s (ary, 1)))
            else
              min (f (n - 1, ary) + abs (s (ary, n) - s (ary, n - 1)),
                   f (n - 2, ary) + abs (s (ary, n) - s (ary, n - 2)))
      in
        (A.update (dp, n, res);
         res)
      end
        
val () =
    (A.tabulate (n, fn i => A.update (ary, i + 1, nextInt ()));
     print (I.toString (f (n, ary)) ^ "\n"))
