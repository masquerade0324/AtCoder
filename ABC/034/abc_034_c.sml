structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

val M = 1000000007 : LI.int

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun pow (_ : LI.int, 0 : LI.int, _ : LI.int) = 1
  | pow (a, b, p) = if b mod 2 = 0 then
                        let val c = pow (a, b div 2, p)
                        in  c * c mod p end
                    else a * pow (a, b - 1, p) mod p

fun P (n : LI.int, k : LI.int) =
    let
        fun P' (i, 0, r) = r mod M
          | P' (i, j, r) = P' (i - 1, j - 1, r * i mod M)
    in
        P' (n, k, 1)
    end

fun C (n, k) = (P (n, k) mod M) * pow (P (k, k), M - 2, M) mod M

val () =
    let
        val (W, H) = (nextLInt (), nextLInt ())
    in
        print (LI.toString (C (W + H - 2, W - 1)) ^ "\n") 
    end
