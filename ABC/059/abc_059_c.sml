structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun solve ([] : LI.int list) _ cnt = cnt
  | solve (x :: xs) sum cnt =
    let
      val sum' = sum + x
    in
      if sum > 0 andalso sum' < 0 orelse sum < 0 andalso sum' > 0 then
        solve xs sum' cnt
      else if sum >= 0 andalso sum' >= 0 then
        solve xs ~1 (cnt + sum' + 1)
      else
        solve xs 1 (cnt - sum' + 1)
    end

val () =
    let
      val n   = LI.toInt (nextInt ())
      val x1  = nextInt ()
      val xs  = List.tabulate (n - 1, fn _ => nextInt ())
      val cnt = if x1 > 0 then
                  LI.min (solve xs x1 0, solve xs ~1 (x1 + 1))
                else if x1 < 0 then
                  LI.min (solve xs 1 (~x1 + 1), solve xs x1 0)
                else
                  LI.min (solve xs 1 1, solve xs ~1 1)
    in
      print (LI.toString cnt ^ "\n")
    end
