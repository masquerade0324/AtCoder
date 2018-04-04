structure SC = StringCvt
structure T  = TextIO
structure V  = Vector

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun dist (x1, y1) (x2, y2) = Int.abs (x1 - x2) + Int.abs (y1 - y2)

val () =
    let
      val (n, m) = (nextInt (), nextInt ())
      val students    = V.tabulate (n, fn _ => (nextInt (), nextInt ()))
      val checkpoints = V.tabulate (m, fn _ => (nextInt (), nextInt ()))
      fun nearestCp student =
          V.foldli (fn (i, cp, (j, min)) =>
                       let
                         val d = dist student cp
                       in
                         if d < min then (i, d) else (j, min)
                       end)
                   (0, 1000000000)
                   checkpoints
    in
      V.app (fn cp => print (Int.toString cp ^ "\n"))
            (V.map (fn (i, _) => i + 1) (V.map nearestCp students))
    end
