structure I  = Int
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun merge ([], ys) = ys
  | merge (xs, []) = xs
  | merge ((x, i)::xs, (y, j)::ys) = if x <= y then (x, i)::merge (xs, (y, j)::ys)
                           else (y, j)::merge ((x, i)::xs, ys)
 
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

fun solve ((x, i)::l) k =
    let
      val i' = LI.fromInt i
    in
      if k <= i' then x else solve l (k - i')
    end

val () =
    let
      val (n, k) = (nextInt (), nextLInt ())
      val l  = List.tabulate (n, fn _ => (nextInt (), nextInt ()))
      val l' = msort l
    in
      print (Int.toString (solve l' k) ^ "\n")
    end
