structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val n = nextInt ()
    val a = nextInt ()
    val b = nextInt ()
    val xs = List.tabulate (n + 1, fn x => x)
    val c2i = fn c => ord c - ord #"0"
    val f = foldl op+ 0 o map c2i o explode o Int.toString
    val (xs', _) = ListPair.unzip (List.filter (fn (p, q) => a <= q andalso q <= b)
                                               (ListPair.zip (xs, map f xs)))
  in
    print (Int.toString (foldl op+ 0 xs') ^ "\n")
  end
