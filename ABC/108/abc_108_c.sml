structure LI  = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

val () =
    let
        val (N, K) = (nextInt (), nextInt ())
    in
        if K mod 2 = 0 then 
            let val i = N div K
                val j = (N + K div 2) div K
            in print (LI.toString (i * i * i + j * j * j) ^ "\n")
            end
        else
            let val i = N div K
            in print (LI.toString (i * i * i) ^ "\n")
            end
    end
