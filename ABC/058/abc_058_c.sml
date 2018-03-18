structure A  = Array
structure C  = Char
structure CV = CharVector
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1)

fun f (i, cnt) = print (CV.tabulate (cnt, fn _ => chr (ord #"a" + i)))

fun solve cnts n =
    if n <= 0 then ()
    else
      let
        val ary = A.array (26, 0)
        val str = next ()
      in
        CV.app (fn c => inc (ary, ord c - ord #"a")) str;
        A.modifyi (fn (i, cnt) => Int.min (cnt, A.sub (ary, i))) cnts;
        solve cnts (n - 1)
      end

val () =
    let
      val n = nextInt ()
      val cnts = A.array (26, 51)
    in
      solve cnts n;
      A.appi f cnts;
      print "\n"
    end
