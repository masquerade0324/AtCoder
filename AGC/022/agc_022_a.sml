structure A  = Array
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1)
fun dec (ary, i) = A.update (ary, i, A.sub (ary, i) - 1)

fun ordinal s =
    let
      val cnts = A.array (26, 0)
      fun update c = inc (cnts, ord c - ord #"a")
      fun isZero (_, cnt) = cnt = 0
      fun f (SOME (i, cnt)) = str (chr (i + ord #"a"))
    in
      print s;
      CV.app update s;
      print (f (A.findi isZero cnts) ^ "\n")
    end

fun special s =
    let
      val cnts = A.array (27, 1)
      val i = ref 24
      val b = ref true
      fun isZero (i', cnt) = cnt = 0 andalso
                             chr (i' + ord #"a") > S.sub (s, !i)
      fun f (SOME (i, cnt)) = str (chr (i + ord #"a"))
      fun g i =
          let
            val j = ref (i + 1)
            val b = ref false
          in
            while !j < 26 do (
              b := (!b orelse A.sub (cnts, !j) = 0);
              j := !j + 1
            );
            !b
          end
    in
      dec (cnts, ord (S.sub (s, 25)) - ord #"a");
      while !i >= 0 andalso !b do (
        if g (ord (S.sub (s, !i)) - ord #"a")
        then
          b := false
        else
          (dec (cnts, ord (S.sub (s, !i)) - ord #"a");
           i := !i - 1)
      );
      print (S.substring (s, 0, !i));
      print (f (A.findi isZero cnts) ^ "\n")
    end

val () =
    let
      val s = next ()
      val n = size s
    in
      if s = "zyxwvutsrqponmlkjihgfedcba" then print "-1\n"
      else if n < 26 then ordinal s
      else special s
    end
