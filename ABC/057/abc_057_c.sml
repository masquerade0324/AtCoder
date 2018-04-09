structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun digit (n : LI.int) : LI.int =
    let
      fun loop (i, m) =
          if m <= n andalso n < 10 * m then i else loop (i + 1, 10 * m)
    in
      loop (1, 1)
    end

fun solve n : LI.int =
    let
      fun loop (i, r) =
          if i * i > n then r
          else
            let
              val j = n div i
            in
              if i * j = n
              then loop (i + 1, LI.min (r, LI.max (digit i, digit j)))
              else loop (i + 1, r)
            end
    in
      loop (1, 11)
    end

val () =
    let
      val n = nextLInt ()
    in
      print (LI.toString (solve n) ^ "\n")
    end
