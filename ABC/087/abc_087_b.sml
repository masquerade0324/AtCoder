structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
    let
      val (a, b, c) = (nextInt (), nextInt (), nextInt ())
      val x = nextInt ()
      val i = ref 0
      val j = ref 0
      val k = ref 0
      val sum = ref 0
    in
      while !i <= a do (
        j := 0;
        while !j <= b do (
          k := 0;
          while !k <= c do (
            if 500 * !i + 100 * !j + 50 * !k = x
            then sum := !sum + 1 else ();
            k := !k + 1
          );
          j := !j + 1
        );
        i := !i + 1
      );
      print (Int.toString (!sum) ^ "\n")
    end
