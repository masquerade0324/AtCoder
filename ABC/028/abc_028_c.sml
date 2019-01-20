structure A  = Array
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1)

val () =
    let
        val A = nextInt ()
        val B = nextInt ()
        val C = nextInt ()
        val D = nextInt ()
        val E = nextInt ()
        val ary = A.array (301, 0)
        val i   = ref 300
        val cnt = ref 0
    in
        inc (ary, A + B + C);
        inc (ary, A + B + D);
        inc (ary, A + B + E);
        inc (ary, A + C + D);
        inc (ary, A + C + E);
        inc (ary, A + D + E);
        inc (ary, B + C + D);
        inc (ary, B + C + E);
        inc (ary, B + D + E);
        inc (ary, C + D + E);
        while !i >= 0 andalso !cnt < 3 do (
            if A.sub (ary, !i) > 0 then cnt := !cnt + 1 else ();
            if !cnt = 3 then print (I.toString (!i) ^ "\n") else ();
            i := !i - 1
        )
    end
