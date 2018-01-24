structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val inf = 1000000000

fun cnt x =
  let
    fun loop (i, acc) = if i mod 2 = 0 then loop (i div 2, acc + 1) else acc
  in
    loop (x, 0)
  end

val () =
  let
    val n   = nextInt ()
    val xs  = List.tabulate (n, fn _ => nextInt ())
    val ans = foldl (Int.min) inf (map cnt xs)
  in
    print (Int.toString ans ^ "\n")
  end
