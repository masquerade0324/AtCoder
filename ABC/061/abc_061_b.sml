structure A  = Array
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1)

fun countRoads roads m =
  if m <= 0 then ()
  else (inc (roads, nextInt () - 1);
        inc (roads, nextInt () - 1);
        countRoads roads (m - 1))

val () =
  let
    val n = nextInt ()
    val m = nextInt ()
    val roads = A.array (n, 0)
  in
    countRoads roads m;
    A.app (fn road => print (Int.toString road ^ "\n")) roads
  end
