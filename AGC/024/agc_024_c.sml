structure A  = Array
structure I  = Int
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)
fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun solve (ary : LI.int array, n) =
    let
      val i   = ref 1
      val cnt = ref 0
    in
      while !i < n do (
        if A.sub (ary, !i) = A.sub (ary, !i - 1) + 1 then ()
        else cnt := !cnt + A.sub (ary, !i - 1);
        i := !i + 1
      );
      cnt := !cnt + A.sub (ary, n - 1);
      !cnt
    end

fun ok (ary : LI.int array, n) =
    let
      val i = ref 0
      val b = ref (A.sub (ary, 0) = 0)
    in
      while !i < n - 1 do (
        if A.sub (ary, !i + 1) - A.sub (ary, !i) > 1
        then b := false else ();
        i := !i + 1
      );
      !b
    end


val () =
    let
      val n   = nextInt ()
      val ary = A.tabulate (n, fn _ => nextLInt ())
    in
      if ok (ary, n)
      then print (LI.toString (solve (ary, n)) ^ "\n")
      else print "-1\n"
    end
