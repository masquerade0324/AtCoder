structure A  = Array
structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1)

fun updateCnts cnts []        = ()
  | updateCnts cnts (c :: cs) = (inc (cnts, ord c - ord #"a");
                                 updateCnts cnts cs)

val () =
    let
      val cnts = A.array (26, 0)
      val cs   = explode (next ())
    in
      updateCnts cnts cs;
      print (if A.all (fn x => x mod 2 = 0) cnts then "Yes\n" else "No\n")
    end
