structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun nextLInt () = valOf (T.scanStream (LargeInt.scan SC.DEC) T.stdIn)

val () =
    let
      val n = nextInt ()
      val x = nextLInt ()
      val l = List.tabulate (n, fn _ => nextLInt ())
      fun solve ([], r)             = r
        | solve ([_], r)            = r
        | solve (t1 :: t2 :: ts, r) =
          let
            val diff = t1 + t2 - x
          in
            if diff <= 0 then solve (t2 :: ts, r)
            else if t1 <= x then solve ((t2 - diff) :: ts, r + diff)
            else solve (0 :: ts, r + diff)
          end
    in
      print (LargeInt.toString (solve (l, 0)) ^ "\n")
    end
