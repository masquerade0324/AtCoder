structure C  = Char
structure CV = CharVector
structure I  = Int
structure L  = List
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun check (k, v) [] = true
  | check (k, v) ((k', v')::l) = if k = k' then v = v' else check (k, v) l

val () =
    let
        val (s, t) = (next (), next ())
        val s2t = ref []
        val t2s = ref []
        val i = ref 0
        val j = ref 0
        val flg = ref true
    in
        while !i < size s andalso !flg do (
            if check (CV.sub (s, !i), CV.sub (t, !i)) (!s2t)
            then s2t := (CV.sub (s, !i), CV.sub (t, !i))::(!s2t)
            else flg := false;
            i := !i + 1
        );
        while !j < size t andalso !flg do (
            if check (CV.sub (t, !j), CV.sub (s, !j)) (!t2s)
            then t2s := (CV.sub (t, !j), CV.sub (s, !j))::(!t2s)
            else flg := false;
            j := !j + 1
        );
        if !flg then print "Yes\n" else print "No\n"
    end
