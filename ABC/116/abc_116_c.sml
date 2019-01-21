structure A  = Array
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure LI = LargeInt
structure S  = String
structure SC = StringCvt
structure T  = TextIO
structure A  = Array

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val garden = A.array (N + 1, 0)
        val h = ref 100
        val c = ref 0
        val res = ref 0
        val i = ref 0
    in
        while !i < N do (
            A.update (garden, !i, nextInt ());
            i := !i + 1
        );
        while !h > 0 do (
            i := 0;
            while !i <= N do (
                if A.sub (garden, !i) >= !h then c := !c + 1
                else
                    if !c >= 1 then (res := !res + 1; c := 0) else ();
                i := !i + 1
            );
            h := !h - 1
        );
        print (I.toString (!res) ^ "\n")
    end
