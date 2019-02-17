structure A  = Array
structure A2 = Array2
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

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1)

val () =
    let
        val (N, M) = (nextInt (), nextInt ())
        val cnt = A.array (M + 1, 0)
        val i = ref 0
        val j = ref 0
        val K = ref 0
    in
        while !i < N do (
            K := nextInt ();
            j := 0;
            while !j < !K do (
                inc (cnt, nextInt ());
                j := !j + 1
            );
            i := !i + 1
        );
        print (I.toString (A.foldl (fn (i, s) => if i = N then s + 1 else s) 0 cnt) ^ "\n")
    end
