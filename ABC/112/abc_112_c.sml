structure L  = List
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val abs = I.abs

val max = I.max

fun get []             = (0, 0, 0)
  | get ((x, y, h)::l) = if h >= 1 then (x, y, h) else get l

val () =
    let
        val n = nextInt ()
        val l = L.tabulate (n, fn _ => (nextInt (), nextInt (), nextInt ()))
        val (xp, yp, hp) = get l
        val cx = ref 0
        val cy = ref 0
        val res = ref (~1, ~1, ~1)
    in
        while !cx <= 100 do (
            cy := 0;
            while !cy <= 100 do (
                let
                    val h = hp + abs (xp - !cx) + abs (yp - !cy)
                in
                    if L.all (fn (xi, yi, hi) =>
                                 hi = max (h - abs (xi - !cx) - abs (yi - !cy), 0))
                             l
                    then res := (!cx, !cy, h)
                    else ()
                end;
                cy := !cy + 1
            );
            cx := !cx + 1
        );
        print (I.toString (#1 (!res)) ^ " " ^
               I.toString (#2 (!res)) ^ " " ^
               I.toString (#3 (!res)) ^ "\n")
    end
