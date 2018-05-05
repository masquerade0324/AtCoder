structure A  = Array
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure LI = LargeInt
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

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

val () =
    let
      val n = nextInt ()
      val primes' = L.filter (fn x => x mod 10 = 1) (mkPrimes 55555)
      val takens  = L.take (primes', n)
    in
      L.app (fn p => print (Int.toString p ^ " ")) takens;
      print "\n"
    end
