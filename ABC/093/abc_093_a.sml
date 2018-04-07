structure C  = Char
structure CV = CharVector
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
  let
    val s = next ()
  in
    if CV.exists (fn c => c = #"a") s andalso
       CV.exists (fn c => c = #"b") s andalso
       CV.exists (fn c => c = #"c") s
    then print "Yes\n"
    else print "No\n"
  end
