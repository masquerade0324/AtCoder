structure C  = Char
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun grd (x, l, u) = if x < l then l else if x > u then u else x

val () =
    let
        val (N, A, B) = (nextInt (), nextInt (), nextInt ())
        val l = L.tabulate (N, fn _ => (next (), grd (nextInt (), A, B)))
        val pos = foldl (fn ((ew, d), p) =>
                            if ew = "East" then p + d else p - d) 0 l
    in
        if pos = 0 then print "0\n"
        else if pos > 0 then print ("East " ^ (I.toString pos) ^ "\n")
        else print ("West " ^ (I.toString (I.abs pos)) ^ "\n")
    end
