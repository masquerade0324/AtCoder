structure A  = Array
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure LI = LargeInt
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val N = nextInt ()

val h = A.array (N + 1, 0)

val _ = A.modifyi (fn (i, _) => if i = 0 then 0 else nextInt ()) h

val dp = A.array (N + 1, 1000000007)

fun solve () =
    let
        val i = ref 3
    in
        A.update (dp, 1, 0);
        A.update (dp, 2, I.abs (A.sub (h, 1) - A.sub (h, 2)));
        while !i <= N do (
            A.update (dp,
                      !i,
                      I.min (A.sub (dp, !i - 2) +
                             I.abs (A.sub (h, !i - 2) - A.sub (h, !i)),
                             A.sub (dp, !i - 1) +
                             I.abs (A.sub (h, !i - 1) - A.sub (h, !i))));
            i := !i + 1
        );
        A.sub (dp, N)
    end

val () = print (I.toString (solve ()) ^ "\n")
