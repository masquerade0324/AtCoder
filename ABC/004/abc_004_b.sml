structure C  = Char
structure I  = Int
structure L  = List
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
        val l1 = L.tabulate (4, fn _ => next ())
        val l2 = L.tabulate (4, fn _ => next ())
        val l3 = L.tabulate (4, fn _ => next ())
        val l4 = L.tabulate (4, fn _ => next ())
    in
        print (S.concatWith " " (rev l4) ^ "\n");
        print (S.concatWith " " (rev l3) ^ "\n");
        print (S.concatWith " " (rev l2) ^ "\n");
        print (S.concatWith " " (rev l1) ^ "\n")
    end
