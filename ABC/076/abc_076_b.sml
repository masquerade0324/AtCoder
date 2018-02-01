structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun solve (n, k, i) = if n = 0 then i
                      else solve (n - 1, k, if i < k then i * 2 else i + k)

val () =
  let
    val n = nextInt ()
    val k = nextInt ()
  in
    print (Int.toString (solve (n, k, 1)) ^ "\n")
  end
