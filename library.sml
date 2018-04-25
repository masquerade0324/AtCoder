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
  fun merge ([], ys) = ys
    | merge (xs, []) = xs
    | merge (x::xs, y::ys) = if x >= y then x::merge (xs, y::ys)
                             else y::merge (x::xs, ys)
  fun split ([], xs, ys)      = (xs, ys)
    | split ([x], xs, ys)     = (x::xs, ys)
    | split (x::y::l, xs, ys) = split (l, x::xs, y::ys)
in
(* マージソート *)
fun msort []  = []
  | msort [x] = [x]
  | msort xs  =
    let
      val (left, right) = split (xs, [], [])
    in
      merge (msort left, msort right)
  end
end
