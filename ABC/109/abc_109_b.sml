structure C  = Char
structure I  = Int
structure L  = List
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun member (x, []) = false
  | member (x, y::ys) = if x = y then true else member (x, ys)

fun f (x, NONE) = NONE
  | f (x, SOME ys) = if member (x, ys) then NONE else SOME (x::ys)

fun check []           = true
  | check [w]          = true
  | check (w1::w2::ws) = if S.sub (w1, size w1 - 1) <> S.sub (w2, 0) then false
                         else check (w2::ws)

val () =
    let
        val N = nextInt ()
        val ws = L.tabulate (N, fn _ => next ())
    in
        case L.foldl f (SOME []) ws of
            NONE     => print "No\n" 
          | SOME ws' => if check ws then print "Yes\n" else print "No\n"
    end
