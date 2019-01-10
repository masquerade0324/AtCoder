structure I  = Int
structure L  = List
structure R  = Real
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextReal () = valOf (T.scanStream R.scan T.stdIn)

local
    fun merge _ ([], ys)             = ys
      | merge _ (xs, [])             = xs
      | merge (op >=) (x::xs, y::ys) =
        if x >= y then x::merge (op >=) (xs, y::ys)
        else y::merge (op >=) (x::xs, ys)
    fun sort _ (0, xs)       = ([], xs)
      | sort _ (1, x::xs)    = ([x], xs)
      | sort (op >=) (n, xs) =
        let
            val (l1, xs1) = sort (op >=) ((n + 1) div 2, xs)
            val (l2, xs2) = sort (op >=) (n div 2, xs1)
        in
            (merge (op >=) (l1, l2), xs2)
        end
in
fun msort (op >=) (xs : real list) =
    let
        val (l, _) = sort (op >=) (length xs, xs)
    in
        l
    end
end

val () =
    let
        val N = nextInt ()
        val RS = L.tabulate (N, fn _ => nextReal ())
        val RS' = msort (op >=) RS
        fun red []        = 0.0
          | red (x::xs)   = x * x + white xs
        and white []      = 0.0
          | white (x::xs) = ~1.0 * x * x + red xs
    in
        print (R.toString (red RS' * Math.pi) ^ "\n")
    end
