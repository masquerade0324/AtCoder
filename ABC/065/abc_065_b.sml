structure A  = Array
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun solve (button, isPushed, i, cnt) =
  if i = 1 then Int.toString cnt
  else if A.sub (isPushed, i) then "-" ^ Int.toString 1
  else (A.update (isPushed, i, true);
        solve (button, isPushed, A.sub (button, i), cnt + 1))

val () =
  let
    val n = nextInt ()
    val button   = A.tabulate (n, fn _ => nextInt () - 1)
    val isPushed = A.array (n, false)
  in
    print (solve (button, isPushed, 0, 0) ^ "\n")
  end
