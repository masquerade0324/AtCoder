structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun merge ([], ys) = ys
  | merge (xs, []) = xs
  | merge (x::xs, y::ys) = if x <= y then x::merge (xs, y::ys)
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

fun solve ([], r)      = r
  | solve ([x], r)     = r + 1
  | solve (x::y::l, r) = if x = y then solve (l, r)
                         else solve (y::l, r + 1)

val () =
    let
      val n = nextInt ()
      val l = List.tabulate (n, fn _ => nextInt ())
    in
      print (Int.toString (solve (msort l, 0)) ^ "\n")
    end
