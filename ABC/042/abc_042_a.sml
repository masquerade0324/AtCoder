structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, c) = (nextInt (), nextInt (), nextInt ())
    in
      if (a, b, c) = (5, 5, 7) orelse
         (a, b, c) = (5, 7, 5) orelse
         (a, b, c) = (7, 5, 5) then print "YES\n" else print "NO\n"
    end   
