structure A  = Array
structure I  = Int
structure L  = List
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

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

fun consec xs : LI.int list =
    let
        fun consec' ([], _)         = []
          | consec' ([_], i)        = [i + 1]
          | consec' (y1::y2::ys, i) = if y1 = y2 then consec' (y2::ys, i + 1)
                                      else (i + 1)::consec' (y2::ys, 0)
    in
        consec' (xs, 0)
    end

val () =
    let
        val (N, M) = (nextInt (), nextLInt ())
        val ary = A.array (N + 1, 0)
        val sum = A.array (N + 1, 0)
        val _ = A.modifyi (fn (i, _) => if i = 0 then 0 else nextLInt ()) ary
        val _ = A.modifyi (fn (i, s) =>
                              if i = 0 then 0
                              else (A.sub (sum, i - 1) + A.sub (ary, i)) mod M) sum
        val l  = msort (op <=) (L.tabulate (N + 1, fn i => A.sub (sum, i)))
        val l' = consec l
    in
        print (LI.toString 
                   (foldl (fn (li, num) => num + li * (li - 1) div 2) 0 l') ^ "\n")
    end
