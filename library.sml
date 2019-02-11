(* Competitive Programming Library in SML
 * @author : masquerade0324 *)

structure LI = LargeInt

(* 2 <= p <= n を満たす素数 p からなるリストを生成：エラトステネスの篩 *)
fun mkPrimes n =
    let
        fun sieve []        = []
          | sieve (p :: xs) =
            if p * p > n then p :: xs
            else p :: sieve (List.filter (fn x => x mod p <> 0) xs)
    in
        2 :: sieve (List.tabulate ((n - 1) div 2, fn i => 2 * i + 3))
    end

(* a^b mod m を計算*)
fun modPow (a : LI.int, b : LI.int, m : LI.int) =
    if b = 0 then 1 mod m
    else
        let
            val c = modPow (a, b div 2, m)
        in
            if b mod 2 = 0 then c * c mod m else c * c mod m * a mod m
        end

(* P (n, k) mod m を計算 *)
fun modPerm (n : LI.int, k : LI.int, m : LI.int) =
    let
        fun perm' (_, 0, r) = r mod m
          | perm' (i, j, r) = perm' (i - 1, j - 1, r * i mod m)
    in
        perm' (n, k, 1)
    end

(* C (n, k) mod m を計算：m は素数、フェルマーの小定理を利用 *)
fun modComb (n : LI.int, k : LI.int, m : LI.int) =
    modPerm (n, k, m) * modPow (modPerm (k, k, m), m - 2, m) mod m

(* 素因数分解 *)
fun primeFactors n =
    let
        fun f (i, m) = if i * i > m then
                           if m <> 1 then [m] else []
                       else
                           if m mod i = 0 then i::f (i, m div i)
                           else f (i + 1, m)
    in
        f (2, n)
    end


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
(* マージソート *)
fun msort (op <=) xs =
    let
        val (l, _) = sort (op <=) (length xs, xs)
    in
        l
    end
end

local
    datatype 'a heap = E
                     | T of int * 'a * 'a heap * 'a heap

    fun rank E                = 0
      | rank (T (n, _, _, _)) = n

    fun makeT (x, l, r) = if rank l >= rank r then T (rank r + 1, x, l, r)
                          else T (rank l + 1, x, r, l)

    fun merge _ (h, E) = h
      | merge _ (E, h) = h
      | merge leq (h1 as T (_, x, l1, r1), h2 as T (_, y, l2, r2)) = 
        if leq (x, y) then makeT (x, l1, merge leq (r1, h2))
        else makeT (y, l2, merge leq (h1, r2))

    fun fromList leq xs = 
        let
            fun mergePairs []           = []
              | mergePairs [h]          = [h]
              | mergePairs (h1::h2::hs) = merge leq (h1, h2)::mergePairs hs
            fun loop []  = E
              | loop [h] = h
              | loop hs  = loop (mergePairs hs)
        in
            loop (map (fn x => T (1, x, E, E)) xs)
        end

    fun toList _ E                  = []
      | toList leq (T (_, x, l, r)) = x::toList leq (merge leq (l, r))
in
(* ヒープソート *)
fun hsort leq xs = toList leq (fromList leq xs)
end

(* Red Black Tree *)
datatype color = R | B

datatype tree = E | T of color * tree * elem * tree

val empty = E

fun member (x, E)              = false
  | member (x, T (_, a, y, b)) = if x < y then member (x, a)
                                 else if x > y then member (x ,b)
                                 else true

fun balance (B, T (R, T (R, a, x, b), y, c), z, d) =
    T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (B, T (R, a, x, T (R, b, y, c)), z, d) =
    T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (B, a , x,T (R, T (R, b, y, c), z, d)) =
    T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (B, a, x, T (R, b, y, T (R, c, z, d))) =
    T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance body = T body

fun insert (x, s) =
    let
        fun ins E = T (R, E, x , E)
          | ins (s as T (color, a, y, b)) =
            if x < y then balance (color, ins a, y, b)
            else if x > y then balance (color, a, y, ins b)
            else s

        val T ( _ , a , y , b ) = ins s
    in
        T (B, a, y, b)
    end
