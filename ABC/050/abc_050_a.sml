structure C  = Char
structure LI = LargeInt
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun prln li = if li >= 0 then print (LI.toString li ^ "\n")
              else print ("-" ^ LI.toString (~li) ^ "\n")

val () =
    let
      val a = nextLInt ()
      val f = if next () = "+" then (op +) else (op -)
      val b = nextLInt ()
    in
      prln (f (a, b))
    end
