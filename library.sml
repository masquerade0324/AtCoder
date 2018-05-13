(* Competitive Programming Library in SML
 * @author : masquerade0324 *)

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
fun hsort leq xs = toList leq (fromList leq xs)
end
