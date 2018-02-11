structure A  = Array
structure C  = Char 
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun cntAlph _   []      = ()
  | cntAlph ary (c::cs) =
  let
    val i = ord c - ord #"a"
  in
    A.update (ary, i, A.sub (ary, i) + 1);
    cntAlph ary cs
  end

fun getMin ary i =
  if i >= 26 orelse i < 0 then NONE
  else
    if A.sub (ary, i) = 0 then SOME i else getMin ary (i + 1)

val () =
  let
    val cs  = explode (next ())
    val ary = A.array (26, 0)
  in
    cntAlph ary cs;
    case getMin ary 0 of
        NONE   => print "None\n"
      | SOME i => print (str (chr (i + ord #"a")) ^ "\n")
  end
