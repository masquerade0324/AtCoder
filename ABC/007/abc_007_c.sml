structure A2 = Array2
structure C  = Char
structure CV = CharVector
structure I  = Int
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

(* queue *)
val empty = ([], [])

fun isEmpty (f, r) = null f

fun checkf ([], r) = (rev r, [])
  | checkf q = q

fun snoc ((f, r), x) = checkf (f, x::r)

fun head ([], _)   = raise Empty
  | head (x::f, r) = x

fun tail ([], _)   = raise Empty
  | tail (x::f, r) = checkf (f, r)

(* main *)
val INF = 1000000

val (R, C)   = (nextInt (), nextInt ())
val (sy, sx) = (nextInt (), nextInt ())
val (gy, gx) = (nextInt (), nextInt ())

val field = A2.array (R + 1, C + 1, #"#")

val d = A2.array (R + 1, C + 1, INF)

val dx = V.fromList [1, 0, ~1, 0]
val dy = V.fromList [0, 1, 0, ~1]

val _ =
    let
        val (i, j) = (ref 1, ref 1)
        val str = ref ""
    in
        while !i <= R do (
            j := 1;
            str := next ();
            while !j <= C do (
                A2.update (field, !i, !j, CV.sub (!str, !j - 1));
                j := !j + 1
            );
            i := !i + 1
        )
    end

fun bfs () =
    let
        val que = ref empty
    in
        que := snoc (!que, (sx, sy));
        A2.update (d, sy, sx, 0);
        while not (isEmpty (!que)) do (
            let
                val (x, y) = head (!que)
                val (x', y') = (ref 0, ref 0)
                val i = ref 0
            in
                que := tail (!que);
                while !i < 4 do (
                    x' := x + V.sub (dx, !i);
                    y' := y + V.sub (dy, !i);
                    if 1 <= !x' andalso !x' <= C andalso
                       0 <= !y' andalso !y' <= R andalso
                       A2.sub (field, !y', !x') = #"." andalso
                       A2.sub (d, !y', !x') = INF
                    then (que := snoc (!que, (!x', !y'));
                          A2.update (d, !y', !x', A2.sub (d, y, x) + 1))
                    else ();
                    i := !i + 1
                )
            end
        );
        A2.sub (d, gy, gx)
    end

val () =
    let
        val res = bfs ()
    in
        print (I.toString res ^ "\n")
    end
