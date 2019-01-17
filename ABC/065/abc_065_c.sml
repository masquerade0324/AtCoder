structure A  = Array
structure LI = LargeInt
structure L  = List
structure SC = StringCvt
structure T  = TextIO

val C = 1000000007 : LI.int

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun fact (n : LI.int) =
    let
        fun fact' (0, r) = r
          | fact' (m, r) = fact' (m - 1, m * r mod C)
    in
        fact' (n, 1)
    end

val () =
    let
        val (N, M) = (nextLInt (), nextLInt ())
    in
        if LI.abs (N - M) > 1 then
            print "0\n"
        else if N = M then
            print (LI.toString (fact N * fact M mod C * 2 mod C) ^ "\n")
        else 
            print (LI.toString (fact N * fact M mod C) ^ "\n")
    end
