structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun split ([], xs, ys)      = (xs, ys)
  | split ([x], xs, ys)     = (x::xs, ys)
  | split (x::y::l, xs, ys) = split (l, x::xs, y::ys)

fun prList []      = print "\n"
  | prList [x]     = print (Int.toString x ^ "\n")
  | prList (x::xs) = (print (Int.toString x ^ " "); prList xs)

val () =
  let
    val n = nextInt ()
    val l = List.tabulate (n, fn _ => nextInt ())
    val (odds, evens) = split (l, [], [])
  in
    prList (if n mod 2 = 0 then evens @ rev odds else odds @ rev evens)
  end
