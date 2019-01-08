structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun i2s i = if i >= 0 then I.toString i else "-" ^ I.toString (~1 * i)

val () =
    let
        val (x1, y1) = (nextInt (), nextInt ())
        val (x2, y2) = (nextInt (), nextInt ())
        val (x3, y3) = (x2 + y1 - y2, y2 + x2 - x1)
        val (x4, y4) = (x3 + x1 - x2, y3 + y1 - y2)
    in
        print (i2s x3 ^ " " ^ i2s y3 ^ " " ^ i2s x4 ^ " " ^ i2s y4 ^ "\n")
    end
