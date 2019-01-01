structure C  = Char
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
        val list = map (fn #"+" => 1 | _ => ~1) (explode (next ()))
        val ans  = foldl (op +) 0 list
    in
        if ans >= 0 then print (I.toString ans ^ "\n")
        else print ("-" ^ I.toString (I.abs ans) ^ "\n")
    end
