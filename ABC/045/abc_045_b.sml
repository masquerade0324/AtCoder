structure C  = Char
structure SC = StringCvt
structure T  = TextIO

exception Unmatched

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun solve #"a" ([], Bs, Cs)      = print "A\n"
  | solve #"a" (x :: As, Bs, Cs) = solve x (As, Bs, Cs)
  | solve #"b" (As, [], Cs)      = print "B\n"
  | solve #"b" (As, y :: Bs, Cs) = solve y (As, Bs, Cs)
  | solve #"c" (As, Bs, [])      = print "C\n"
  | solve #"c" (As, Bs, z :: Cs) = solve z (As, Bs, Cs)
  | solve _ _ = raise Unmatched

val () =
    let
      val cardsA = explode (next ())
      val cardsB = explode (next ())
      val cardsC = explode (next ())
    in
      solve #"a" (cardsA, cardsB, cardsC)
    end
