structure A  = Array
structure L  = List
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val upd = A.update

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

fun lt ((_, _, x), (_, _, y)) = x <= y

val () =
    let
        val (n, m) = (nextInt (), nextInt ())
        val cnts = A.array (n, 0)
        val odrs = A.array (m, 0)
        val l  = L.tabulate (m, fn i => (i, nextInt () - 1, nextInt ()))
        val l' = msort lt l
        val i = ref 0
    in
        L.app (fn (i, p, _) => (upd (cnts, p, A.sub (cnts, p) + 1);
                                upd (odrs, i, A.sub (cnts, p)))) l';
        L.app (fn (i, p, _) => print (
                                  SC.padLeft #"0" 6 (I.toString (p + 1)) ^
                                  SC.padLeft #"0" 6 (I.toString (A.sub (odrs, i))) ^
                                  "\n")) l
    end
