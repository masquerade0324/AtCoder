structure C  = Char
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun update x []          = [(x, 1)]
  | update x ((y, i)::l) = if x = y then (y, i + 1)::l else (y, i)::update x l

fun updates n l = if n = 0 then l else updates (n - 1) (update (next ()) l)

val () =
    let
        val n = nextInt ()
        val (name, cnt) =
            foldl (fn ((a, i), (b, j)) => if i > j then (a, i) else (b, j))
                  ("", 0) (updates n [])
    in
        print (name ^ "\n")
    end
