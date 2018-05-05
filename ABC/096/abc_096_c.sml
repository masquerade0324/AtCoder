structure A  = Array2
structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure LI = LargeInt
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
      val (h, w) = (nextInt (), nextInt ())
      val s = A.array (h + 2, w + 2, false)
      val i = ref 1
      val a = ref 1
      val b = ref 1
      val res = ref true
    in
      while !i <= h do (
        CV.appi (fn (j, #"#") => A.update (s, !i, j + 1, true)
                  | _         => ())
                (next ());
        i := !i + 1
      );
      while !a <= h do (
        b := 1;
        while !b <= w do (
          if A.sub (s, !a, !b)
          then res := (!res andalso (A.sub (s, !a - 1, !b) orelse
                                     A.sub (s, !a, !b - 1) orelse
                                     A.sub (s, !a, !b + 1) orelse
                                     A.sub (s, !a + 1, !b)))
          else ();
          b := !b + 1
        );
        a := !a + 1
      );
      print (if !res then "Yes\n" else "No\n") 
    end
