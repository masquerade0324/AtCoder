structure C  = Char
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

fun member (x, [])    = false
  | member (x, y::ys) = if x = y then true else member (x, ys)

val () =
    let
        val s = next ()
        val n = size s
        val K = nextInt ()
        fun loop (i, m, r) =
            if i >= m then r
            else loop (i + 1,
                       m,
                       L.tabulate (n - i, fn j => S.substring (s, j, i + 1)) :: r)
        val ss = L.concat (loop (0, I.min (n, K), []))
        val ss' = foldl (fn (s, l) => if member (s, l) then l else s::l) [] (ss)
    in
        print (L.nth (msort (op <=) ss', K - 1) ^ "\n")
    end
