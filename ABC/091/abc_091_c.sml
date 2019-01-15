structure A = Array
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

fun geqRed ((_, y1), (_, y2)) = y1 >= y2

fun leqBlue ((x1, _), (x2, _)) = x1 <= x2

val () =
    let
        val N = nextInt ()
        val rs = L.tabulate (N, fn _ => (nextInt (), nextInt ()))
        val bs = L.tabulate (N, fn _ => (nextInt (), nextInt ()))
        val rs' = V.fromList (msort geqRed rs)
        val bs' = V.fromList (msort leqBlue bs)
        val usedRed = Array.array (N, false)
        val usedBlue = Array.array (N, false)
        val cnt = ref 0
        val i = ref 0
        val j= ref 0
    in
        while !i < N do (
            j := 0;
            while !j < N andalso not (A.sub (usedBlue, !i)) do (
                if A.sub (usedRed, !j) then ()
                else
                    let val (xr, yr) = V.sub (rs', !j)
                        val (xb, yb) = V.sub (bs', !i)
                    in if xr < xb andalso yr < yb
                       then (cnt := !cnt + 1;
                             A.update (usedRed, !j, true);
                             A.update (usedBlue, !i, true))
                       else ()
                    end;
                j := !j + 1
            );
            i := !i + 1
        );
        print (I.toString (!cnt) ^ "\n")
    end
