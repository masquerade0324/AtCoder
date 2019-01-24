structure R  = Real
structure SC = StringCvt
structure T  = TextIO

fun nextReal () = valOf (T.scanStream R.scan T.stdIn)

val () =
    let
        val (xa, ya) = (nextReal (), nextReal ())
        val (xb, yb) = (nextReal (), nextReal ())
        val (xc, yc) = (nextReal (), nextReal ())
        val s = R.abs ((xb - xa) * (yc - ya) - (yb - ya) * (xc - xa)) / 2.0
    in
        print (R.toString s ^ "\n")
    end
