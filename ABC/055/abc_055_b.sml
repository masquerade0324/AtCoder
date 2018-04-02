structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val c = 1000000007
      val n = nextInt ()
      fun fact n : LargeInt.int =
          let
            fun loop (0, r) = r
              | loop (m, r) = loop (m - 1, m * r mod c)
          in
            loop (n, 1)
          end
    in
      print (LargeInt.toString (fact (LargeInt.fromInt n)) ^ "\n")
    end
