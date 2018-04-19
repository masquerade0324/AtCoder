structure A  = Array
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun dec ary i = A.update (ary, i, A.sub (ary, i) - 1)

fun pow (a, 0) : LargeInt.int = 1
  | pow (a, n) : LargeInt.int = a * pow (a, n - 1) mod 1000000007

val () =
    let
      val n = nextInt ()
      val l = L.tabulate (n, fn _ => nextInt ())
      val cnts = if n mod 2 = 0 
                 then A.tabulate (n + 1, fn i => if i mod 2 = 0 then 0 else 2)
                 else A.tabulate (n + 1, fn i => if i = 0 then 1
                                                 else if i mod 2 = 0 then 2
                                                 else 0)
    in
      L.app (dec cnts) l;
      if A.all (fn cnt => cnt = 0) cnts
      then print (LargeInt.toString (pow (2, n div 2)) ^ "\n")
      else print "0\n"
    end
