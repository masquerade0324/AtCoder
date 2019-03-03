structure A  = Array
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure LI = LargeInt
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (A, B, K) = (nextInt (), nextInt (), nextInt ())
        val i = ref 100
        val cnt = ref 0
    in
        while !cnt < K do (
            if A mod !i = 0 andalso B mod !i = 0
            then cnt := !cnt + 1 else ();
            i := !i - 1
        );
        print (I.toString (!i + 1) ^ "\n")
    end
