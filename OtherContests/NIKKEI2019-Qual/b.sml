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
        val N = nextInt ()
        val (A, B, C) = (next (), next (), next ())
        val i = ref 0
        val cnt = ref 0
    in
        while !i < N do (
            if CV.sub (A, !i) = CV.sub (B, !i) andalso
               CV.sub (B, !i) = CV.sub (C, !i) then ()
            else if CV.sub (A, !i) <> CV.sub (B, !i) andalso
                    CV.sub (B, !i) <> CV.sub (C, !i) andalso
                    CV.sub (C, !i) <> CV.sub (A, !i) then cnt := !cnt + 2
            else cnt := !cnt + 1;
            i := !i + 1
        );
        print (I.toString (!cnt) ^ "\n")
    end
