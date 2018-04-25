structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun member _ []       = false
  | member x (h :: t) = if x = h then true else member x t

fun i2c i = chr (ord #"0" + i)

fun solve digits i =
    let
      val cs = explode (Int.toString i)
    in
      if List.all (fn c => member c digits) cs then i else solve digits (i + 1)
    end

val () =
    let
      val n = nextInt ()
      val k = nextInt ()
      val inputs = List.tabulate (k, fn _ => nextInt ())
      val digits = map i2c (List.filter (fn x => not (member x inputs))
                                        (List.tabulate (10, fn i => i)))
    in
      print (Int.toString (solve digits n) ^ "\n")
    end
