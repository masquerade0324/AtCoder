structure C  = Char
structure S  = String
structure SC = StringCvt
structure T  = TextIO

exception Undefined

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun stoi s = valOf (LargeInt.fromString s)

val () =
    let
      val digits = map str (explode (next ()))
      fun f []               = raise Undefined
        | f [s]              = [stoi s]
        | f (s1 :: s2 :: ss) = f (s1 ^ s2 :: ss) @
                               map (fn x => x + stoi s1) (f (s2 :: ss))
    in
      print (LargeInt.toString (foldl (op+) 0 (f digits)) ^ "\n")
    end
