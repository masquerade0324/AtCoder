structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val n = nextInt ()
        val h = n div 3600
        val m = (n - h * 3600) div 60
        val s = n - h * 3600 - m * 60
    in
        print ((if h >= 10 then "" else "0") ^ I.toString h ^ ":");
        print ((if m >= 10 then "" else "0") ^ I.toString m ^ ":");
        print ((if s >= 10 then "" else "0") ^ I.toString s ^ "\n")
    end
