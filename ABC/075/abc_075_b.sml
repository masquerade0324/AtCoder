structure A2 = Array2
structure C  = Char
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm =
  SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val dydx = [(~1, ~1), (~1, 0), (~1, 1),
            ( 0, ~1),          ( 0, 1),
            ( 1, ~1), ( 1, 0), ( 1, 1)]  

fun input (ary, h, w) =
  let
    val y = ref 1
  in
    while !y <= h do (
      let
        val x = ref 1
        val str = next ()
      in
        while !x <= w do (
          A2.update (ary, !y, !x, S.sub (str, !x-1));
          x := !x + 1)
      end;
      y := !y + 1)
  end

fun output (ary, h, w) =
  let
    val y = ref 1
    val s = ref ""
  in
    while !y <= h do (
      let
        val x = ref 1
      in
        while !x <= w do (
          s := !s ^ str (A2.sub (ary, !y, !x));
          x := !x + 1)
      end;
      s := !s ^ "\n";
      y := !y + 1);
    print (!s)
  end

fun cntNbr ary (y, x) =
  let
    fun f ((dy, dx), sum) = if A2.sub (ary, y+dy, x+dx) = #"#"
                            then sum + 1 else sum
  in
    if A2.sub (ary, y, x) = #"#" then #"#"
    else chr ((foldl f 0 dydx) + ord #"0")
  end

val () =
  let
    val h = nextInt ()
    val w = nextInt ()
    val ary = A2.array (h+2, w+2, #".")
  in
    input (ary, h, w);
    A2.modifyi A2.RowMajor (fn (y, x, _) => cntNbr ary (y, x))
               {base=ary, row=1, col=1, nrows=SOME h, ncols=SOME w};
    output (ary, h, w)
  end
