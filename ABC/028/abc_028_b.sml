structure A  = Array
structure C  = Char
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
        val cnts = A.array (6, 0)
        val l = map (fn c => ord c - ord #"A") (explode (next ()))
    in
        L.app (fn i => A.update (cnts, i, A.sub (cnts, i) + 1)) l;
        A.appi (fn (i, cnt) => if i <= 4 then print (I.toString cnt ^ " ")
                               else print (I.toString cnt ^ "\n")) cnts
    end
