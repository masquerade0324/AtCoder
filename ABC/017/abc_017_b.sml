structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun isChoku []           = true
  | isChoku [c]          = c = #"o" orelse c = #"k" orelse c = #"u"
  | isChoku (c1::c2::cs) = if c1 = #"c" andalso c2 = #"h" then isChoku cs
                           else if c1 = #"o" orelse
                                   c1 = #"k" orelse
                                   c1 = #"u" then isChoku (c2::cs)
                           else false

val () =
    let
        val cs = explode (next ())
    in
        print (if isChoku cs then "YES\n" else "NO\n")
    end
