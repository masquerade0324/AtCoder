structure A  = Array
structure I  = Int
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1)

fun rewrite (ary, i, m) = if A.sub (ary, i) = m
                          then (A.update (ary, i, 0); i)
                          else rewrite (ary, i + 1, m)

val () =
    let
        val n = nextInt ()
        val os = A.array (100001, 0)
        val es = A.array (100001, 0)
        val v = V.tabulate (n, fn _ => nextInt ())
        val _ = V.appi (fn (i, v) => if i mod 2 = 1 then inc (os, v)
                                     else inc (es, v)) v
        val (mo1, me1) = (A.foldl I.max 0 os, A.foldl I.max 0 es)
        val (o1, e1) = (rewrite (os, 0, mo1), rewrite (es, 0, me1))
        val (mo2, me2) = (A.foldl I.max 0 os, A.foldl I.max 0 es)
    in
        if o1 <> e1 then print (I.toString (n - mo1 - me1) ^ "\n")
        else print (I.toString (I.min (n - mo1 - me2, n - mo2 - me1)) ^ "\n")
    end
