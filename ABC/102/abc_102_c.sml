structure I  = Int
structure L  = List
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

local
  fun merge _ ([], ys)             = ys
    | merge _ (xs, [])             = xs
    | merge (op <=) (x::xs, y::ys) =
      if x <= y then x::merge (op <=) (xs, y::ys)
      else y::merge (op <=) (x::xs, ys)
  fun sort _ (0, xs)       = ([], xs)
    | sort _ (1, x::xs)    = ([x], xs)
    | sort (op <=) (n, xs) =
      let
        val (l1, xs1) = sort (op <=) ((n + 1) div 2, xs)
        val (l2, xs2) = sort (op <=) (n div 2, xs1)
      in
        (merge (op <=) (l1, l2), xs2)
      end
in
fun msort (op <=) xs =
    let
      val (l, _) = sort (op <=) (length xs, xs)
    in
      l
    end
end


val () =
    let
        val N = nextInt ()
        val l = L.tabulate (N, fn i => nextLInt () - LI.fromInt i - 1)
        val v = V.fromList (msort (op <=) l)
        val b = V.sub (v, N div 2)
        val ans = foldl (fn (a, s) => s + LI.abs (a - b)) 0 l
    in
        print (LI.toString ans ^ "\n")
    end
