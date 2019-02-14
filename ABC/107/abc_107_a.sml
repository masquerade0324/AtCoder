structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (N, i) = (nextInt (), nextInt ())
    in
        print (I.toString (N - i + 1) ^ "\n")
    end
