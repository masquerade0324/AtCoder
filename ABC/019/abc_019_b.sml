structure C  = Char
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun f ([], i, l)         = l
  | f ([c], i, l)        = (str c ^ I.toString (i + 1)):: l
  | f (c1::c2::cs, i, l) = if c1 = c2 then f (c2::cs, i + 1, l)
                           else f (c2::cs, 0, (str c1 ^ I.toString (i + 1)) :: l)

val () =
    let
        val s = next ()
    in
        print (concat (rev (f ((explode s), 0, []))) ^ "\n")
    end
