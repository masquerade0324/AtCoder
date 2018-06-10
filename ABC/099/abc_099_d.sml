structure A  = Array2
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure LI = LargeInt
structure S  = String
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun inc (ary, i, j) = A.update (ary, i, j, A.sub (ary, i, j) + 1)

val () =
    let
      val (n, c) = (nextInt (), nextInt ())
      val aryD = A.tabulate A.RowMajor (c, c, fn (_, _) => nextInt ())
      val aryC = A.tabulate A.RowMajor (n, n, fn (_, _) => nextInt () - 1) 
      val ary  = A.array (3, c, 0)
      val i = ref 0
      val j = ref 0
      val k = ref 0
      val min = ref 1000000000
    in
      A.appi A.RowMajor (fn (i, j, x) => inc (ary, (i + j) mod 3, x))
             {base=aryC, row=0, col=0, nrows=SOME n, ncols=SOME n};
      while !i < c do (
        j := 0;
        while !j < c do (
          k := 0;
          while !k < c do (
            if !i <> !j andalso !j <> !k andalso !k <> !i 
            then
              min := I.min (!min,
                            V.foldl (op +) 0 (V.mapi (fn (pre, x) => A.sub (aryD, pre, !i) * x) (A.row (ary, 0)))
                            + V.foldl (op +) 0 (V.mapi (fn (pre, x) => A.sub (aryD, pre, !j) * x) (A.row (ary, 1)))
                            + V.foldl (op +) 0 (V.mapi (fn (pre, x) => A.sub (aryD, pre, !k) * x) (A.row (ary, 2))))
            else
              ();
            k := !k + 1
          );
          j := !j + 1
        );
        i := !i + 1
      );
      print (I.toString (!min) ^ "\n")
    end
