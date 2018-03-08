structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun count (rem0, rem2, remOw) i =
  case i mod 4 of
      0 => (rem0 + 1, rem2, remOw)
    | 2 => (rem0, rem2 + 1, remOw)
    | _ => (rem0, rem2, remOw + 1)

val () =
  let
    val n = nextInt ()
    val l = List.tabulate (n, fn _ => nextInt ())
    val (r0, r2, rOw) = foldl (fn (x, rem) => count rem x) (0, 0, 0) l
  in
    print (if r0 >= n div 2 then "Yes\n"
           else if r2 >= n - 2 * r0 then "Yes\n"
           else "No\n")
  end
