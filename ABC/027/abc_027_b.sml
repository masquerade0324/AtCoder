structure I  = Int
structure L  = List
structure R  = Real
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val l = L.tabulate (N, fn _ => nextInt ())
        val sum = foldl (op +) 0 l
        val ave = sum div N
        val (_, sum', cnt) =
            foldl (fn (a, (s, r, cnt)) =>
                      (s + a, r + ave, if s + a = r + ave then cnt else cnt + 1))
                  (0, 0, 0) l
    in
        if sum <> sum' then print "-1\n" else print (I.toString cnt ^ "\n")
    end
