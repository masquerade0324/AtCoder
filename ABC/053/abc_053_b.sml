structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
      val s = next ()
      val n = size s
      val i = ref 0
      val j = ref (n - 1)
    in
      while !i < n do (
      )
    end
