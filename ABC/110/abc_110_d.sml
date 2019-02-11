structure I  = Int
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun consec xs : LI.int list =
    let
        fun consec' ([], _)         = []
          | consec' ([_], i)        = [i + 1]
          | consec' (y1::y2::ys, i) = if y1 = y2 then consec' (y2::ys, i + 1)
                                      else (i + 1)::consec' (y2::ys, 0)
    in
        consec' (xs, 0)
    end

val MOD = 1000000007 : LI.int

fun modPow (a : LI.int, b : LI.int, m : LI.int) =
    if b = 0 then 1 mod m
    else
        let
            val c = modPow (a, b div 2, m)
        in
            if b mod 2 = 0 then c * c mod m else c * c mod m * a mod m
        end

fun modPerm (n : LI.int, k : LI.int, m : LI.int) =
    let
        fun perm' (_, 0, r) = r mod m
          | perm' (i, j, r) = perm' (i - 1, j - 1, r * i mod m)
    in
        perm' (n, k, 1)
    end

fun modComb (n : LI.int, k : LI.int, m : LI.int) =
    modPerm (n, k, m) * modPow (modPerm (k, k, m), m - 2, m) mod m

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

val () =
    let
        val (N, M) = (nextLInt (), nextInt ())
        val l   = consec (primeFactors M)
        val res = foldl (fn (i, m) =>
                            m * modComb (i + N - 1, N - 1, MOD) mod MOD) 1 l
    in
        print (LI.toString res ^ "\n")
    end
