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

fun kaisa (x1::x2::xs) = (x2 - x1)::kaisa (x2::xs)
  | kaisa [x] = []

val () =
    let
        val (N, M) = (nextInt (), nextInt ())
        val xs = msort (op <=) (L.tabulate (M, fn _ => nextInt ()))
    in
        if N >= M then print "0\n"
        else print (I.toString (foldl (op +) 0
                                      (L.drop (msort (op >=) (kaisa xs), N - 1))) ^ "\n")
    end
