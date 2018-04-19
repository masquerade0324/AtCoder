structure C  = Char
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun revStr s = implode (rev (explode s))

val () =
    let
      val dream   = revStr "dream"
      val dreamer = revStr "dreamer"
      val erase   = revStr "erase"
      val eraser  = revStr "eraser"
      val s       = revStr (next ())
      fun solve str =
          let
            val n = size str
          in
            if n = 0 then "YES"
            else if S.isPrefix dream   str then solve (substring (str, 5, n - 5))
            else if S.isPrefix dreamer str then solve (substring (str, 7, n - 7))
            else if S.isPrefix erase   str then solve (substring (str, 5, n - 5))
            else if S.isPrefix eraser  str then solve (substring (str, 6, n - 6))
            else "NO"
          end
    in
      print (solve s ^ "\n")
    end
