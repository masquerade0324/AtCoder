structure A2 = Array2
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val ary = A2.fromList (L.tabulate (N, fn _ => explode (next ())))
        val ary' = A2.tabulate A2.RowMajor
                               (N, N, fn (i, j) => A2.sub (ary, N - j - 1, i))
        val i = ref 0
    in
        while !i < N do (
            let
                val vec = A2.row (ary', !i)
            in
                V.app (fn c => T.output1 (T.stdOut, c)) vec;
                print "\n"
            end;
            i := !i + 1)
    end
