structure SC = StringCvt
structure T  = TextIO

exception Undefined

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun nextLInt () = valOf (T.scanStream (LargeInt.scan SC.DEC) T.stdIn)

fun solve []       = raise Undefined
  | solve [(x, y)] = x + y
  | solve ((x1, y1) :: (x2, y2) :: pairs) = 
    let
      val m = LargeInt.max ((x1 + x2 - 1) div x2, (y1 + y2 - 1) div y2)
    in
      solve ((m * x2, m * y2) ::  pairs)
    end
      
val () =
    let
      val n = nextInt ()
      val pairs = List.tabulate (n, fn _ => (nextLInt (), nextLInt ()))
    in
      print (LargeInt.toString (solve pairs) ^ "\n")
    end
