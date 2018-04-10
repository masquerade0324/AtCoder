structure A =  Array
structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun merge ([], ys) = ys
  | merge (xs, []) = xs
  | merge (x::xs, y::ys) = if x >= y then x::merge (xs, y::ys)
                           else y::merge (x::xs, ys)
                                         
fun split ([], xs, ys)      = (xs, ys)
  | split ([x], xs, ys)     = (x::xs, ys)
  | split (x::y::l, xs, ys) = split (l, x::xs, y::ys)
                                    
fun msort []  = []
  | msort [x] = [x]
  | msort xs  =
    let
      val (left, right) = split (xs, [], [])
    in
      merge (msort left, msort right)
    end

val () =
    let
      val n  = nextInt ()
      val xs = List.tabulate (n, fn _ => nextInt ())
      fun solve (a, b) []        = a - b
        | solve (a, b) [x]       = (a + x) - b
        | solve (a, b) (x::y::l) = solve (a + x, b + y) l
    in
      print (Int.toString (solve (0, 0) (msort xs)) ^ "\n")
    end
