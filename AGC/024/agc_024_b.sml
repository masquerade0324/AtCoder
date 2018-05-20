structure A  = Array
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
      val n = nextInt ()
      val p = A.tabulate (n, fn _ => nextInt () - 1)
      val q = A.array (n, 0)
      val _ = A.appi (fn (i, x) => A.update (q, x, i)) p
      val i   = ref 1
      val cnt = ref 1
      val max = ref 1
    in
      while !i < n do (
        if A.sub (q, !i - 1) < A.sub (q, !i)
        then cnt := !cnt + 1
        else cnt := 1;
        max := I.max (!max, !cnt);
        i := !i + 1
      );
      print (I.toString (n - !max) ^ "\n")
    end
