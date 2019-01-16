structure C  = Char
structure L  = List
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val subs = substring

val toA = fn c => if c = #"?" then #"a" else c

fun min (s : string, t) = if s <= t then s else t

fun judge (s, t) =
    let
        val n = size t
        val i = ref 0
        val res = ref true
    in
        while !i < n do (
            res := (!res andalso (S.sub (s, !i) = S.sub (t, !i) orelse
                                  S.sub (s, !i) = #"?"));
            i := !i + 1
        );
        !res
    end

val () =
    let
        val S' = next ()
        val T  = next ()
        val ns = size S'
        val nt = size T
        val z  = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
        val l  = L.tabulate (ns - nt + 1, fn i => i)
        val kouho =
            foldl (fn (i, ss) =>
                      if judge (subs (S', i, nt), T)
                      then let val s1 = S.map toA (subs (S', 0, i))
                               val s2 = S.map toA (subs (S', i + nt, ns - i - nt))
                           in (s1 ^ T ^ s2) :: ss
                           end
                      else ss) [] l
    in
        case kouho of
            [] => print "UNRESTORABLE\n"
          | _  => print (foldl min z kouho ^ "\n")
    end
