structure A  = Array
structure C  = Char
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))
fun next () = valOf (T.scanStream scan T.stdIn)

val () =
  let
    val cs = explode (next ())
    val occurs = A.array (26, 0)
    fun inc ary i = A.update (ary, i, A.sub (ary, i) + 1)
  in
    L.app (fn c => inc occurs (ord c - ord #"a")) cs;
    print (if A.all (fn x => x <= 1) occurs then "yes\n" else "no\n")
  end
