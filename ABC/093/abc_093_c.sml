structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val (a, b, c) = (nextInt (), nextInt (), nextInt ())
    val (max, mdl, min) =
        if      a >= b andalso b >= c then (a, b, c)
        else if a >= c andalso c >= b then (a, c, b)
        else if b >= a andalso a >= c then (b, a, c)
        else if b >= c andalso c >= a then (b, c, a)
        else if c >= a andalso a >= b then (c, a, b)
        else (* c >= b andalso b >= a *)   (c, b, a)
    val res =
        if (max - mdl) mod 2 = 0
        then
          if (max - min) mod 2 = 0
          then (max - mdl) div 2 + (max - min) div 2
          else (max - mdl) div 2 + (max - min) div 2 + 2
        else
          if (max - min - 1) mod 2 = 0
          then (max - mdl) div 2 + (max - min - 1) div 2 + 1
          else (max - mdl) div 2 + (max - min - 1) div 2 + 3
  in
    print (Int.toString res ^ "\n")
  end
