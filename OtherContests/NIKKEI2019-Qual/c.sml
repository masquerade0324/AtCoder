structure A  = Array
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure LI = LargeInt
structure S  = String
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun toStr i = if i >= 0 then LI.toString i else "-" ^ LI.toString (~i)

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

val () =
    let
        val N = nextInt ()
        val ABs  = L.tabulate (N, fn _ => (nextLInt (), nextLInt ()))
        val ABs' = V.fromList (msort (fn ((a1, b1), (a2, b2)) =>
                                         a1 + b1 >= a2 + b2) ABs)
        val taka = ref 0
        val aoki = ref 0
        val i = ref 0
    in
        while !i < N do (
            if !i mod 2 = 0 then
                taka := !taka + #1 (V.sub (ABs', !i))
            else
                aoki := !aoki + #2 (V.sub (ABs', !i));
            i := !i + 1
        );
        print (toStr (!taka - !aoki) ^ "\n")
    end
