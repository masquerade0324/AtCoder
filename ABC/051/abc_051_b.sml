structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
      val (k, s)    = (nextInt (), nextInt ())
      val (x, y, z) = (ref 0, ref 0, ref 0)
      val cnt       = ref 0
    in
      x := 0;
      while !x <= k do (
        y := 0;
        while !y <= k do (
          z := s - !x - !y;
          if 0 <= !z andalso !z <= k
          then cnt := !cnt + 1 else ();
          y := !y + 1
        );
        x := !x + 1
      );
      print (I.toString (!cnt) ^ "\n")
    end
