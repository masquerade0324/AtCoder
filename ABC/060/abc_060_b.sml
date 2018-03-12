structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val (a, b, c) = (nextInt (), nextInt (), nextInt ())
  in
    print (if L.exists (fn i => i = c) (L.tabulate (b, fn i => a * i mod b))
           then "YES\n" else "NO\n")
  end
