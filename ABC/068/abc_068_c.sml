structure A  = Array
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun mem (x : ''a , []) = false
  | mem (x, y::ys)     = if x = y then true else mem (x, ys)

val (N, M) = (nextInt (), nextInt ())

val S = A.array (N, false)
val D = A.array (N, false)

val _ =
    let
        val i = ref 0
        val (u, v) = (ref 0, ref 0)
    in
        while !i < M do (
            u := nextInt () - 1;
            v := nextInt () - 1;
            if !u = 0     then A.update (S, !v, true) else ();
            if !v = N - 1 then A.update (D, !u, true) else ();
            i := !i + 1
        )
    end

val () =
    let
        val i = ref 1
        val b = ref false
    in
        while (!i < N - 1) do (
            if A.sub (S, !i) andalso A.sub (D, !i) then b := true else ();
            i := !i + 1
        );
        if !b then print "POSSIBLE\n" else print "IMPOSSIBLE\n"
    end
