structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun f ((x, y, a), (xmin, xmax, ymin, ymax)) =
    if      a = 1 then (I.max (x, xmin), xmax, ymin, ymax)
    else if a = 2 then (xmin, I.min (x, xmax), ymin, ymax)
    else if a = 3 then (xmin, xmax, I.max (y, ymin), ymax)
    else (* a = 4 *)   (xmin, xmax, ymin, I.min (y, ymax))

val () =
    let
      val (w, h, n) = (nextInt (), nextInt (), nextInt ())
      val l = L.tabulate (n, fn _ => (nextInt (), nextInt (), nextInt ()))
      val (xmin, xmax, ymin, ymax) = foldl f (0, w, 0, h) l
    in
      print (I.toString (I.max (xmax - xmin, 0) * I.max (ymax - ymin, 0)) ^ "\n")
    end
