structure A  = Array
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val joshi  = A.array (N, ~1)
        val salary = A.array (N, 0)
        val i = ref 1
        val j = ref 0
        val max = ref 0
        val min = ref 1000000000
    in
        while !i < N do (
            A.update (joshi, !i, nextInt () - 1);
            i := !i + 1
        );
        i := N -1;
        while !i >= 0 do (
            j := !i + 1;
            max := 0;
            min := 1000000000;
            while !j < N do (
                if A.sub (joshi, !j) = !i
                then (max := I.max (!max, A.sub (salary, !j));
                      min := I.min (!min, A.sub (salary, !j)))
                else ();
                j := !j + 1
            );
            if !min = 1000000000 then min := 0 else ();
            A.update (salary, !i, !max + !min + 1);
            i := !i - 1
        );
        print (I.toString (A.sub (salary, 0)) ^ "\n")
    end
