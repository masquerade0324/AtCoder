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
      val alpha = "abcdefghijklmnopqrstuvwxyz"
      val n     = nextInt ()
      val s     = next ()
      val list  = L.tabulate (n - 1, fn i => i + 1)
      fun eq (c : char) = fn c' => c = c'
      fun cutAndCnt i =
          let
            val left  = S.substring (s, 0, i)
            val right = S.substring (s, i, n - i)
          in
            CV.foldl
               (fn (c, acc) => 
                   if CV.exists (eq c) left andalso CV.exists (eq c) right
                   then acc + 1 else acc) 0 alpha
          end
    in
      print (I.toString (foldl I.max 0 (map cutAndCnt list)) ^ "\n")
    end
