structure A  = Array
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun println i = print (I.toString i ^ "\n")

local
  fun le ((_, x), (_, y)) = x <= y
  fun merge ([], ys) = ys
    | merge (xs, []) = xs
    | merge (x::xs, y::ys) = if le (x, y) then x::merge (xs, y::ys)
                             else y::merge (x::xs, ys)
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
      val xs = L.tabulate (n, fn i => (i, nextInt ()))
      val ys = A.fromList (msort xs)
      val (_, med1) = A.sub (ys, n div 2 - 1)
      val (_, med2) = A.sub (ys, n div 2)
      val ary = A.array (n, 0)
    in
      A.appi (fn (i, (j, _)) => A.update (ary, j, i)) ys;
      A.app (fn i => println (if i < n div 2 then med2 else med1)) ary
    end
