structure A  = Array
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun update cnts a =
    A.modifyi (fn (i, cnt) => if i mod a = 0 then cnt + 1 else cnt) cnts

fun solve cnts n = if n <= 0 then A.foldl (op +) 0 cnts
                   else (update cnts (nextInt ());
                         solve cnts (n - 1))

val () =
    let
      val n    = nextInt ()
      val day  = nextInt ()
      val rest = nextInt ()
      val cnts = A.array (day, 0)
    in
      print (Int.toString (solve cnts n + rest) ^ "\n")
    end
