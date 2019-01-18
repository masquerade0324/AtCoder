structure A  = Array
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun println i = if i >= 0 then print (I.toString i ^ "\n")
                else print ("-" ^ I.toString (~i) ^ "\n")

val () =
    let
        val N = nextInt ()
        val ary  = A.array (N + 1, 0)
        val aryO = A.array (N + 1, 0)
        val aryE = A.array (N + 1, 0)
        val sumO = A.array (N + 2, 0)
        val sumE = A.array (N + 2, 0)
        val takas = A.array (N + 1, ~1000000)
        val aoki = ref (~1000000)
        val i = ref 1
        val j = ref 1
    in
        while !i <= N do (
            A.update (ary, !i, nextInt ());
            if !i mod 2 = 1 then
                A.update (aryO, !i, A.sub (ary, !i))
            else
                A.update (aryE, !i, A.sub (ary, !i));
            i := !i + 1
        );
        i := 1;
        while !i <= N do (
            A.update (sumO, !i + 1, A.sub (sumO, !i) + A.sub (aryO, !i));
            A.update (sumE, !i + 1, A.sub (sumE, !i) + A.sub (aryE, !i));
            i := !i + 1
        );
        i := 1;
        while !i <= N do (
            aoki := ~1000000;
            j := 1;
            while !j < !i do (
                if !j mod 2 = 1 then
                    if !aoki < A.sub (sumE, !i + 1) - A.sub (sumE, !j)
                    then
                        (aoki := A.sub (sumE, !i + 1) - A.sub (sumE, !j);
                         A.update (takas, !i, A.sub (sumO, !i + 1) -
                                              A.sub (sumO, !j)))
                    else ()
                else
                    if !aoki < A.sub (sumO, !i + 1) - A.sub (sumO, !j)
                    then
                        (aoki := A.sub (sumO, !i + 1) - A.sub (sumO, !j);
                         A.update (takas, !i, A.sub (sumE, !i + 1) -
                                              A.sub (sumE, !j)))
                    else ();
                j := !j + 1
            );
            j := !i + 1;
            while !j <= N do (
                if !i mod 2 = 1 then
                    if !aoki < A.sub (sumE, !j + 1) - A.sub (sumE, !i)
                    then
                        (aoki := A.sub (sumE, !j + 1) - A.sub (sumE, !i);
                         A.update (takas, !i, A.sub (sumO, !j + 1) -
                                              A.sub (sumO, !i)))
                    else ()
                else
                    if !aoki < A.sub (sumO, !j + 1) - A.sub (sumO, !i)
                    then
                        (aoki := A.sub (sumO, !j + 1) - A.sub (sumO, !i);
                         A.update (takas, !i, A.sub (sumE, !j + 1) -
                                              A.sub (sumE, !i)))
                    else ();
                j := !j + 1
            );
            i := !i + 1
        );
        (* print "--- sumO ---\n";
        A.app (fn i => print (I.toString i ^ "\n")) sumO;
        print "--- sumE ---\n";
        A.app (fn i => print (I.toString i ^ "\n")) sumE;
        print "--- takas ---\n";
        A.app (fn i => print (I.toString i ^ "\n")) takas;
        print "--- Answer ---\n"; *)
        println (A.foldl I.max ~1000000 takas)
    end
