structure C  = Char
structure T  = TextIO
structure S  = String
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun tokenize () = S.tokens C.isSpace (valOf (T.inputLine T.stdIn))

val () =
  let
    val _  = nextInt ()
    val k  = nextInt ()
    val ss = (T.inputLine T.stdIn; tokenize ())
    val xs = map (valOf o Int.fromString) ss
    fun f x = Int.min (x, k - x)
  in
    print (Int.toString (2 * foldl (op +) 0 (map f xs)) ^ "\n")
  end
