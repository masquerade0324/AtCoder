structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

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

fun numbering ([], n)                = []
  | numbering ([(i, x)], n)          = [(i, n)]
  | numbering ((i, x)::(j, y)::l, n) =
    if x = y then (i, n)::numbering ((j, y)::l,n)
    else (i, n)::numbering ((j, y)::l, n + 1)

val () =
    let
        val N   = nextInt ()
        val l   = L.tabulate (N, fn i => (i, nextInt ()))
        val l'  = numbering (msort (fn ((_, x), (_, y)) => x <= y) l, 0)
        val l'' = msort (fn ((i, _), (j, _)) => i <= j) l'
    in
        L.app (fn (_, x) => print (I.toString x ^ "\n")) l''
    end
