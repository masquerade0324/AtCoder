structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun mkFactors (i, n) = if i * i <= n then
                           if n mod i = 0 then i::(n div i)::mkFactors (i + 1, n)
                           else mkFactors (i + 1, n)
                       else []

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
        val (N, M) = (nextInt (), nextInt ())
        val factors = msort (op <=) (mkFactors (1, M))
        val res = foldl (fn (fact, max) =>
                            if M div fact >= N then fact else max) 1 factors
    in
        print (I.toString res ^ "\n")
    end
