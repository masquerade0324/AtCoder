structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun unique []           = []
  | unique [x]          = [x]
  | unique (x1::x2::xs) = if x1 = x2 then unique (x2::xs) else x1::unique (x2::xs)

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
        val n = nextInt ()
        val x1::x2::xs = unique (msort (op >=)
                                       (L.tabulate (n, fn _ => nextInt ())))
    in
        print (I.toString x2 ^ "\n")
    end
