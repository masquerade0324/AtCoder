structure A  = Array
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (N, D, K) = (nextInt (), nextInt (), nextInt ())
        val lrs = A.tabulate (D, fn _ => (nextInt (), nextInt ()))
        val sts = A.tabulate (K, fn _ => (nextInt (), nextInt ()))
        val k = ref 0
    in
        while !k < K do (
            let
                val d = ref 0
                val (s, t) = A.sub (sts, !k)
                val arrived = ref false
                val pos = ref s
            in
                if s < t then
                    while !d < D andalso not (!arrived) do (
                        let
                            val (l, r) = A.sub (lrs, !d)
                        in
                            if l <= !pos andalso !pos <= r then pos := r else ()
                        end;
                        if !pos >= t then arrived := true else ();
                        d := !d + 1
                    )
                else
                    while !d < D andalso not (!arrived) do (
                        let
                            val (l, r) = A.sub (lrs, !d)
                        in
                            if l <= !pos andalso !pos <= r then pos := l else ()
                        end;
                        if !pos <= t then arrived := true else ();
                        d := !d + 1
                    );
                print (I.toString (!d) ^ "\n");
                k := !k + 1
            end
        )
    end
