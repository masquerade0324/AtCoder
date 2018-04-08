structure A  = Array2
structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (n, m) = (nextInt (), nextInt ())
      val aryA = A.fromList (List.tabulate (n, fn _ => explode (next ())))
      val aryB = A.fromList (List.tabulate (m, fn _ => explode (next ())))
      fun match (i, j) =
          A.foldi
            A.RowMajor
            (fn (x, y, b, acc) => acc andalso A.sub (aryA, i + x, j + y) = b)
            true
            {base=aryB, row=0, col=0, nrows=NONE, ncols=NONE}
    in
      if A.foldi
           A.RowMajor
           (fn (i, j, _, acc) => acc orelse match (i, j))
           false
           {base=aryA, row=0, col=0,
            nrows=SOME (n - m + 1), ncols=SOME (n - m + 1)}
      then print "Yes\n"
      else print "No\n"
    end
