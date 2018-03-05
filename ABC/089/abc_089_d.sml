structure A  = Array
structure A2 = Array2
structure C  = Char
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun dist (i1, j1) (i2, j2) = Int.abs (i2 - i1) + Int.abs (j2 - j1)

fun calRuiseki ary ruiseki n d =
  let
    val i = ref 1
  in
    while !i <= d do (
      i := !i + 1
    );
    while !i <= n do (
      A.update (ruiseki, !i , A.sub (ruiseki, !i - d) +
                              dist (A.sub (ary, !i - d)) (A.sub (ary, !i)));
      i := !i + 1
    )
  end

fun solve ruiseki q =
  if q <= 0 then ()
  else
    let
      val (l, r) = (nextInt (), nextInt ())
    in
      print (Int.toString (A.sub (ruiseki, r) - A.sub (ruiseki, l)) ^ "\n");
      solve ruiseki (q - 1)
    end

val () =
  let
    val (h, w, d) = (nextInt (), nextInt (), nextInt ())
    val ary  = A.array (h * w + 1, (~1, ~1))
    val ary2 = A2.tabulate A2.RowMajor
                           (h, w, (fn pnt => A.update (ary, nextInt (), pnt)))
    val ruiseki = A.array (h * w + 1, 0)
    val q = nextInt ()
  in
    calRuiseki ary ruiseki (h * w) d;
    solve ruiseki q
  end
