structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

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

val () =
    let
        val (n, k) = (nextInt (), nextInt ())
        val hs = L.tabulate (n, fn _ => nextInt ())
        val vec = V.fromList (msort (op <=) hs)
        val i = ref 0
        val ans = ref 1000000000
    in
        while !i + k - 1 < n do (
            ans := I.min (!ans, V.sub (vec, !i + k - 1) - V.sub (vec, !i));
            i := !i + 1
        );
        print (I.toString (!ans) ^ "\n")
    end
