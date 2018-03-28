structure A  = Array2
structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun i2s i = if i >= 0 then Int.toString i else "-" ^ Int.toString (~i)

fun makeArray a =
    let
      val i = ref 1
    in
      while !i < 1024 do (
        A.update (a, !i, 0, !i div 1   mod 2);
        A.update (a, !i, 1, !i div 2   mod 2);
        A.update (a, !i, 2, !i div 4   mod 2);
        A.update (a, !i, 3, !i div 8   mod 2);
        A.update (a, !i, 4, !i div 16  mod 2);
        A.update (a, !i, 5, !i div 32  mod 2);
        A.update (a, !i, 6, !i div 64  mod 2);
        A.update (a, !i, 7, !i div 128 mod 2);
        A.update (a, !i, 8, !i div 256 mod 2);
        A.update (a, !i, 9, !i div 512 mod 2);
        i := !i + 1
      )
    end

fun solve n (f, p, a) =
    let
      val i   = ref 1
      val j   = ref 0
      val cnt = ref 0
      val sum = ref 0
      val max = ref ~1000000001
    in
      while !i < 1024 do (
        j := 0; sum := 0;
        while !j < n do (
          cnt := 0;
          if A.sub (a, !i, 0) * A.sub (f, !j, 0) = 1
          then cnt := !cnt + 1 else ();
          if A.sub (a, !i, 1) * A.sub (f, !j, 1) = 1
          then cnt := !cnt + 1 else ();
          if A.sub (a, !i, 2) * A.sub (f, !j, 2) = 1
          then cnt := !cnt + 1 else ();
          if A.sub (a, !i, 3) * A.sub (f, !j, 3) = 1
          then cnt := !cnt + 1 else ();
          if A.sub (a, !i, 4) * A.sub (f, !j, 4) = 1
          then cnt := !cnt + 1 else ();
          if A.sub (a, !i, 5) * A.sub (f, !j, 5) = 1
          then cnt := !cnt + 1 else ();
          if A.sub (a, !i, 6) * A.sub (f, !j, 6) = 1
          then cnt := !cnt + 1 else ();
          if A.sub (a, !i, 7) * A.sub (f, !j, 7) = 1
          then cnt := !cnt + 1 else ();
          if A.sub (a, !i, 8) * A.sub (f, !j, 8) = 1
          then cnt := !cnt + 1 else ();
          if A.sub (a, !i, 9) * A.sub (f, !j, 9) = 1
          then cnt := !cnt + 1 else ();
          sum := !sum + A.sub (p, !j, !cnt);
          j := !j + 1
        );
        max := Int.max (!max, !sum);
        i := !i + 1
      );
      !max
    end

val () =
  let
    val n  = nextInt ()
    val f = A.tabulate A.RowMajor (n, 10, fn _ => nextInt ())
    val p = A.tabulate A.RowMajor (n, 11, fn _ => nextInt ())
    val a = A.array (1024, 10, 0)
    val _ = makeArray a
  in
    print (i2s (solve n (f, p, a)) ^ "\n")
  end
