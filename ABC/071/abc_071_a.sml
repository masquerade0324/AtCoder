structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

                       
val () =
  let
    val (x, a, b) = (nextInt (), nextInt (), nextInt ())
    fun abs i = if i >= 0 then i else ~i
  in
    print (if abs (x - a) < abs (x - b) then "A\n" else "B\n")
  end
