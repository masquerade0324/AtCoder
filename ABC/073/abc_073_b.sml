structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    fun printlnInt i = print (Int.toString i ^ "\n")
    val n  = nextInt ()
    val xs = List.tabulate (n, fn _ => (nextInt (), nextInt ()))
  in
    printlnInt (foldl (fn ((l, r), sum) => sum + r - l + 1) 0 xs)
  end
