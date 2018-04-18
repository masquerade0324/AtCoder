structure A  = Array
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1 : LargeInt.int)

(* make a prime list by "Sieve of Eratosthenes" *)
fun mkPrimes n =
    let
      fun sieve []        = []
        | sieve (p :: xs) = if p * p > n then p :: xs
                            else p :: sieve (L.filter (fn x => x mod p <> 0) xs)
    in
      2 :: sieve (L.tabulate ((n - 1) div 2, fn i => 2 * i + 3))
    end

fun factorize [] n        = []
  | factorize (p :: ps) n =
    if n mod p = 0 then p ::factorize (p :: ps) (n div p) else factorize ps n

val () =
    let
      val const  = 1000000007 : LargeInt.int
      val n      = nextInt ()
      val primes = mkPrimes n
      val cnts   = A.array (n + 1, 0 : LargeInt.int)
      (* xss is a list of prime factors *)
      val xss    = map (factorize primes) (List.tabulate (n, fn i => i + 1))
      fun update cnts []        = ()
        | update cnts (p :: ps) = (inc (cnts, p); update cnts ps)
    in
      L.app (update cnts) xss;
      print (LargeInt.toString
               (A.foldl (fn (cnt, r) => r * (cnt + 1) mod const) 1 cnts) ^
             "\n")
    end
