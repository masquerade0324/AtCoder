structure I  = Int
structure L  = List
structure R  = Real
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextReal () = valOf (T.scanStream R.scan T.stdIn)

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
        val (N, K) = (nextInt (), nextInt ())
        val l = L.tabulate (N, fn _ => nextReal ())
        val v = V.fromList (msort (op >=) l)
        val i = ref (K - 1)
        val rate = ref 0.0
    in
        while !i >= 0 do (
            rate := (!rate + V.sub (v, !i)) / 2.0;
            i := !i - 1
        );
        print (R.toString (!rate) ^ "\n")
    end
