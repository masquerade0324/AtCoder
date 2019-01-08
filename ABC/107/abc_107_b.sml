structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (H, W) = (nextInt (), nextInt ())
        val field  = L.tabulate (H, fn _ => next ())
        val field' = ref (L.filter (fn r => not (CV.all (fn c => c = #".") r)) field)
        val j = ref 0
    in
        while !j < W do (
            if L.all (fn r => S.sub (r, !j) = #".") (!field')
            then field' := map (fn r => CV.update (r, !j, #" ")) (!field')
            else ();
            j := !j + 1
        );
        field' := map (fn r => S.translate (fn #" " => "" | c => str c) r) (!field');
        L.app (fn r => print (r ^ "\n")) (!field')
    end
