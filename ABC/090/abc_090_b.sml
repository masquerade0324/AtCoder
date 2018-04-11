structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b) = (nextInt (), nextInt ())
      val l = L.tabulate (b - a + 1, fn i => a + i)
      fun isPalin i =
          let 
            val str = Int.toString i
          in
            str = implode (rev (explode str))
          end
    in
      print (Int.toString (length (L.filter isPalin l)) ^ "\n")
    end
