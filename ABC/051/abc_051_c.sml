structure CV = CharVector
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (sx, sy) = (nextInt (), nextInt ())
      val (tx, ty) = (nextInt (), nextInt ())
    in
      print (
        CV.tabulate (ty - sy, fn _ => #"U") ^
        CV.tabulate (tx - sx, fn _ => #"R") ^
        CV.tabulate (ty - sy, fn _ => #"D") ^
        CV.tabulate (tx - sx, fn _ => #"L") ^
        "L" ^
        CV.tabulate (ty - sy + 1, fn _ => #"U") ^
        CV.tabulate (tx - sx + 1, fn _ => #"R") ^
        "D" ^
        "R" ^
        CV.tabulate (ty - sy + 1, fn _ => #"D") ^
        CV.tabulate (tx - sx + 1, fn _ => #"L") ^
        "U\n"
      )
    end
