structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val X  = nextInt ()
        val ns = L.tabulate (1000, fn i => i + 1)
        fun extract []      = 0
          | extract (n::ns) = if n * n * n * n = X then n else extract ns
    in
        print (I.toString (extract ns) ^ "\n")
    end
