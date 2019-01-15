structure A  = Array
structure C  = Char
structure CV = CharVector
structure I  = Int
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val S = next ()
        val s = substring (S, 1, N - 1)
        val nums = A.array (N, 0)
        val i = ref 1
    in
        A.modifyi (fn (i, _) =>
                      if i = 0
                      then CV.foldl (fn (c, n) => if c = #"E" then n + 1 else n) 0 s
                      else A.sub (nums, i - 1) +
                           (if CV.sub (S, i - 1) = #"W" then 1 else 0) + 
                           (if CV.sub (S, i) = #"E" then ~1 else 0)) nums;
        print (I.toString (A.foldl I.min 1000000000 nums) ^ "\n")
    end
