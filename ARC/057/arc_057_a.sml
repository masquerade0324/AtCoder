structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val () =
    let
        val (A, K) = (nextLInt (), nextLInt ())
        val money = ref A
        val NICHO = 2000000000000
        val day = ref 0
    in
        if K = 0 then print (LI.toString (NICHO - A) ^ "\n")
        else
            (while !money < NICHO do (
                 money := (K + 1) * !money + 1;
                 day := !day + 1 
             );
             print (LI.toString (!day) ^ "\n"))
    end
