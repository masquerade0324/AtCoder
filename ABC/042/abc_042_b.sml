structure C  = Char
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

local
  fun merge ([], ys) : string list = ys
    | merge (xs, []) = xs
    | merge (x::xs, y::ys) = if x <= y then x::merge (xs, y::ys)
                             else y::merge (x::xs, ys)
  fun split ([], xs, ys)      = (xs, ys)
    | split ([x], xs, ys)     = (x::xs, ys)
    | split (x::y::l, xs, ys) = split (l, x::xs, y::ys)
in
(* マージソート *)
fun msort []  = []
  | msort [x] = [x]
  | msort xs  =
    let
      val (left, right) = split (xs, [], [])
    in
      merge (msort left, msort right)
  end
end

val () =
    let
      val n = nextInt ()
      val _ = nextInt ()
      val ss = List.tabulate (n, fn _ => next ())
    in
      print (String.concat (msort ss) ^ "\n")
    end
   
