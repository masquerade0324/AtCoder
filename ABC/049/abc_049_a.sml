structure C  = Char
structure L  = List
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
      val vowels = ["a", "i", "u", "e", "o"]
      val str    = next ()
    in
      print (if L.exists (fn v => str = v) vowels then "vowel\n" else "consonant\n")
    end
