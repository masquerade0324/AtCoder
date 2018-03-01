structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun input (n, xs) = if n <= 0 then xs else input (n - 1, nextInt () :: xs)

val () =
  let
    val n  = nextInt ()
    val xs = input (n, [])
    val d  = foldl Int.max 0 xs - foldl Int.min 1000 xs
  in
    print (Int.toString d ^ "\n")
  end
