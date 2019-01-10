structure I  = Int
structure R  = Real
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val (n, m) = (nextInt () mod 12, nextInt ())
        val degL = (R.fromInt n * 60.0 + R.fromInt m) * 0.5
        val degS = R.fromInt m * 6.0
        val deg =
             if degL > degS then
                 if degL - degS < 180.0 then degL - degS else 360.0 + degS - degL
             else
                 if degS - degL < 180.0 then degS - degL else 360.0 + degL - degS
    in
        print (Real.toString deg ^ "\n")
    end
