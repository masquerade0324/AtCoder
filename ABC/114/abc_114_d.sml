structure A  = Array
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1)

fun mkPrimes n =
    let
        fun sieve []        = []
          | sieve (p :: xs) =
            if p * p > n then p :: xs
            else p :: sieve (List.filter (fn x => x mod p <> 0) xs)
    in
        2 :: sieve (List.tabulate ((n - 1) div 2, fn i => 2 * i + 3))
    end

val primes = mkPrimes 100

val factors = A.array (101, 0)

fun factorize n =
    let
        fun f (1, _)     = ()
          | f (m, [])    = ()
          | f (m, p::ps) =
            if m mod p = 0 then (inc (factors, p); f (m div p, p::ps))
            else f (m, ps)
    in
        if n = 1 then () else (f (n, primes); factorize (n - 1))
    end

val () =
    let
        val N = nextInt ()
        val _ = factorize N
        val cnt74 = A.foldl (fn (i, s) => if i >= 74 then s + 1 else s) 0 factors
        val cnt24 = A.foldl (fn (i, s) => if i >= 24 then s + 1 else s) 0 factors
        val cnt14 = A.foldl (fn (i, s) => if i >= 14 then s + 1 else s) 0 factors
        val cnt4  = A.foldl (fn (i, s) => if i >= 4 then s + 1 else s) 0 factors
        val cnt2  = A.foldl (fn (i, s) => if i >= 2 then s + 1 else s) 0 factors
    in
        print (I.toString (cnt74 +
                           cnt24 * (cnt2 - 1) +
                           cnt14 * (cnt4 - 1) +
                           cnt4 * (cnt4 - 1) div 2 * (cnt2 - 2)) ^ "\n")
    end
