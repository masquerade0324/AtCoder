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

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val (N, M) = (nextInt (), nextInt ())
val bridge = A.tabulate (M, fn _ => (nextInt () - 1, nextInt () - 1))
val answer = A.array (M, 0 : LI.int)

val par  = A.tabulate (N, fn i => i)
val rank = A.array (N, 0)
val cnt  = A.array (N, 1 : LI.int)

fun find x = if A.sub (par, x) = x then x
             else
                 let val res = find (A.sub (par, x))
                 in  A.update (par, x, res);
                     res
                 end

fun unite (x, y) =
    let
        val (x', y') = (find x, find y)
        val cntx = A.sub (cnt, x')
        val cnty = A.sub (cnt, y')
    in
        if x' = y' then false
        else
            (if A.sub (rank, x') < A.sub (rank, y')
             then A.update (par, x', y')
             else (A.update (par, y', x');
                   if A.sub (rank, x') = A.sub (rank, y')
                   then A.update (rank, x', A.sub (rank, x') + 1) else ());
             A.update (cnt, x', cntx + cnty);
             A.update (cnt, y', cntx + cnty);
             true)
             
    end

fun same (x, y) = find x = find y

val () =
    let
        val i = ref (M - 1)
    in
        A.update (answer, M - 1, LI.fromInt N * LI.fromInt (N - 1) div 2);
        while !i >= 1 do (
            let
                val (x, y) = A.sub (bridge, !i)
                val cntx = A.sub (cnt, find x)
                val cnty = A.sub (cnt, find y)
            in
                if unite (x, y) then
                    A.update (answer, !i - 1, A.sub (answer, !i) - cntx * cnty)
                else
                    A.update (answer, !i - 1, A.sub (answer, !i))
            end;
            i := !i - 1
        );
        A.app (fn li => print (LI.toString li ^ "\n")) answer
    end
