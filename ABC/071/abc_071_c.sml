structure LI = LargeInt
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

fun get ([] : int list) = (NONE, [])
  | get [x] = (NONE, [])
  | get (x1 :: x2 :: xs) =
    if x1 = x2 then (SOME x1, xs) else get (x2 :: xs)

val () =
    let
      val n  = nextInt ()
      val xs = msort (List.tabulate (n, fn _ => nextInt ()))
      val (a, ys) = get xs
      val (b, _)  = get ys
      val sq = case (a, b) of
                   (SOME s, SOME t) => LI.fromInt s * LI.fromInt t
                 | (_, _)           => 0
    in
      print (LI.toString sq ^ "\n")
    end
