structure C  = Char
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun member (x, l) = L.exists (fn y => x = y) l

fun unique (x, l) = if member (x, l) then l else x::l

val () =
    let
        val s = next ()
        val k = nextInt ()
        val l = if k > size s then []
                else L.tabulate (size s - k + 1, fn i => substring (s, i, k))
    in
        print (I.toString (length (foldl unique [] l)) ^ "\n")
    end
