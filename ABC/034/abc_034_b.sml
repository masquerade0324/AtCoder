structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val n = nextInt ()
    in
        print (I.toString (if n mod 2 = 0 then n - 1 else n + 1) ^ "\n")
    end
