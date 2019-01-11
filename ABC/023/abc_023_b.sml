structure C  = Char
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun shugei (0, n, _) = shugei (1, n, "b")
  | shugei (i, n, acc) = if i > n then (i - 1, acc)
                         else if i mod 3 = 0 then shugei (i + 1, n, "b" ^ acc ^ "b")
                         else if i mod 3 = 1 then shugei (i + 1, n, "a" ^ acc ^ "c")
                         else shugei (i + 1, n, "c" ^ acc ^ "a")

val () =
    let
        val n = nextInt ()
        val s = next ()
    in
        if n mod 2 = 0 then print "-1\n"
        else
            let val (k, acc) = shugei (0, (n - 1) div 2, "")
            in if s <> acc then print "-1\n"
               else print (I.toString k ^ "\n")
            end
    end
