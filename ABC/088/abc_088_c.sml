structure A2 = Array2
structure I  = Int
structure SC = StringCvt
structure T  = TextIO

val sub = A2.sub

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val c = A2.tabulate A2.RowMajor (3, 3, fn (_, _) => nextInt ())
    in
        if  sub (c, 0, 0) - sub (c, 0, 1) = sub (c, 1, 0) - sub (c, 1, 1) andalso
            sub (c, 1, 0) - sub (c, 1, 1) = sub (c, 2, 0) - sub (c, 2, 1) andalso
            sub (c, 2, 0) - sub (c, 2, 1) = sub (c, 0, 0) - sub (c, 0, 1) andalso

            sub (c, 0, 1) - sub (c, 0, 2) = sub (c, 1, 1) - sub (c, 1, 2) andalso
            sub (c, 1, 1) - sub (c, 1, 2) = sub (c, 2, 1) - sub (c, 2, 2) andalso
            sub (c, 2, 1) - sub (c, 2, 2) = sub (c, 0, 1) - sub (c, 0, 2) andalso
          
            sub (c, 0, 2) - sub (c, 0, 0) = sub (c, 1, 2) - sub (c, 1, 0) andalso
            sub (c, 1, 2) - sub (c, 1, 0) = sub (c, 2, 2) - sub (c, 2, 0) andalso
            sub (c, 2, 2) - sub (c, 2, 0) = sub (c, 0, 2) - sub (c, 0, 0) andalso

            sub (c, 0, 0) - sub (c, 1, 0) = sub (c, 0, 1) - sub (c, 1, 1) andalso
            sub (c, 0, 1) - sub (c, 1, 1) = sub (c, 0, 2) - sub (c, 1, 2) andalso
            sub (c, 0, 2) - sub (c, 1, 2) = sub (c, 0, 0) - sub (c, 1, 0) andalso

            sub (c, 1, 0) - sub (c, 2 ,0) = sub (c, 1, 1) - sub (c, 2, 1) andalso
            sub (c, 1, 1) - sub (c, 2, 1) = sub (c, 1, 2) - sub (c, 2, 2) andalso
            sub (c, 1, 2) - sub (c, 2, 2) = sub (c, 1, 0) - sub (c, 2 ,0) andalso

            sub (c, 2, 0) - sub (c, 0, 0) = sub (c, 2, 1) - sub (c, 0, 1) andalso
            sub (c, 2, 1) - sub (c, 0, 1) = sub (c, 2, 2) - sub (c, 0, 2) andalso
            sub (c, 2, 2) - sub (c, 0, 2) = sub (c, 2, 0) - sub (c, 0, 0)
        then print "Yes\n" else print "No\n"
    end
