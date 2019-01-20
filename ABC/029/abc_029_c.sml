structure C  = Char
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun generate []        = []
  | generate (cs::css) = (#"a"::cs)::(#"b"::cs)::(#"c"::cs)::generate css

fun loop (i, n, res) = if i >= n then res
                       else loop (i + 1, n, generate res)

val () =
    let
        val N = nextInt()
    in
        L.app (fn cs => print (implode (rev cs) ^ "\n"))
              (loop (0, N, [[]]))
    end
