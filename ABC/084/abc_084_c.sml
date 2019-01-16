structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun calArrivalTime l =
    foldl (fn ((c, s, f), t) => if t < s then s + c
                                else if t mod f = 0 then t + c
                                else t - t mod f + f + c) 0 l

fun calAll [] = [0]
  | calAll (h::t) = calArrivalTime (h::t) :: calAll t

val () =
  let
      val N = nextInt ()
      val l = L.tabulate (N - 1, fn _ => (nextInt (), nextInt (), nextInt ()))
      val times = calAll l
  in
      L.app (fn t => print (I.toString t ^ "\n")) times
  end
