structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val n = nextInt ()
    val ans = if n >= 64 then 64
              else if n >= 32 then 32
              else if n >= 16 then 16
              else if n >= 8 then 8
              else if n >= 4 then 4
              else if n >= 2 then 2
              else 1
  in
    print (Int.toString ans ^ "\n")
  end
