structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun f (n, ak, ak1, ak2) = if n = 1 then ak mod 10007
                          else if n = 2 then ak1 mod 10007
                          else if n = 3 then ak2 mod 10007
                          else f (n - 1, ak1, ak2, (ak + ak1 + ak2) mod 10007)

fun g n = f (n, 0, 0, 1)

val () =
    let
        val n = nextInt ()
    in
        print (I.toString (g n) ^ "\n")
    end
