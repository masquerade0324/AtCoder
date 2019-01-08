structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val A = N div 4
        val B = N div 7
        val a = ref 0
        val b = ref 0
        val flag = ref false
    in
        while !a <= A do (
            b := 0;
            while !b <= B do (
                if 4 * !a + 7 * !b = N then flag := true else ();
                b := !b + 1
            );
            a := !a + 1
        );
        if !flag then print "Yes\n" else print "No\n"
    end
