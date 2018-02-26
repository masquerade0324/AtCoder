structure C  = Char
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

exception Unsolved

fun solve (s, n) =
    if n <= 1 then 0
    else
      let
        val m = n div 2
      in
        if substring (s, 0, m) = substring (s, m, m) then n
        else solve (s, n - 2)
      end

val () =
  let
    val s = next ()
  in
    print (Int.toString (solve (s, size s - 2)) ^ "\n")
  end
