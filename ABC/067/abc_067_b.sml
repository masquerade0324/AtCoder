structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun merge ([], ys) = ys
  | merge (xs, []) = xs
  | merge (x::xs, y::ys) = if x >= y then x::merge (xs, y::ys)
                           else y::merge (x::xs, ys)
 
fun split ([], xs, ys)      = (xs, ys)
  | split ([x], xs, ys)     = (x::xs, ys)
  | split (x::y::l, xs, ys) = split (l, x::xs, y::ys)
 
fun msort []  = []
  | msort [x] = [x]
  | msort xs  =
  let
    val (left, right) = split (xs, [], [])
  in
    merge (msort left, msort right)
  end

fun sum xs = foldl op+ 0 xs

val () =
  let
    val (n, k) = (nextInt (), nextInt ())
    val l = List.tabulate (n, fn _ => nextInt ())
  in
    print (Int.toString (sum (List.take (msort l, k))) ^ "\n")
  end
