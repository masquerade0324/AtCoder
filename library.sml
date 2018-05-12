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
