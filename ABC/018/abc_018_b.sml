structure C  = Char
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val ss = substring

val () =
    let
        val s = ref (next ())
        val len = size (!s)
        val n = nextInt ()
        val i = ref 0
    in
        while !i < n do (
            let val (l, r) = (nextInt (), nextInt ())
            in s := ss (!s, 0, l - 1) ^
                    (implode o rev o explode) (ss (!s, l - 1, r - l + 1)) ^
                    ss (!s, r, len - r)
            end;
            i := !i + 1
        );
        print (!s ^ "\n")
    end
