structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

local
  fun merge ([], ys) = ys
    | merge (xs, []) = xs
    | merge ((i, x)::xs, (j, y)::ys) =
      if x >= y then (i, x)::merge (xs, (j, y)::ys)
      else (j, y)::merge ((i, x)::xs, ys)
  fun split ([], xs, ys)      = (xs, ys)
    | split ([x], xs, ys)     = (x::xs, ys)
    | split (x::y::l, xs, ys) = split (l, x::xs, y::ys)
in
(* マージソート *)
fun msort []  = []
  | msort [x] = [x]
  | msort xs  =
    let
      val (left, right) = split (xs, [], [])
    in
      merge (msort left, msort right)
  end
end

val () =
    let
      val n  = nextInt ()
      val xs = L.tabulate (n, fn i => (i + 1, nextInt ()))
    in
      L.app (fn (i, x) => print (I.toString i ^ "\n")) (msort xs)
    end
