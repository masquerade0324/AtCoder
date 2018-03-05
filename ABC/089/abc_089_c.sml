structure A  = Array
structure C  = Char
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm)) 
fun next () = valOf (T.scanStream scan T.stdIn)
fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun input (cnts : LargeInt.int array) n =
  if n <= 0 then ()
  else
    let
      val head = S.sub (next (), 0)
    in
      if head = #"M" then A.update (cnts, 0, A.sub (cnts, 0) + 1)
      else if head = #"A" then A.update (cnts, 1, A.sub (cnts, 1) + 1)
      else if head = #"R" then A.update (cnts, 2, A.sub (cnts, 2) + 1)
      else if head = #"C" then A.update (cnts, 3, A.sub (cnts, 3) + 1)
      else if head = #"H" then A.update (cnts, 4, A.sub (cnts, 4) + 1)
      else ();
      input cnts (n - 1)
    end

val () =
  let
    val n = nextInt ()
    val cnts = A.array (5, LargeInt.fromInt 0)
  in 
    input cnts n;
    print (LargeInt.toString
             (A.sub (cnts, 0) * A.sub (cnts, 1) * A.sub (cnts, 2) +
              A.sub (cnts, 0) * A.sub (cnts, 1) * A.sub (cnts, 3) +
              A.sub (cnts, 0) * A.sub (cnts, 1) * A.sub (cnts, 4) +
              A.sub (cnts, 0) * A.sub (cnts, 2) * A.sub (cnts, 3) +
              A.sub (cnts, 0) * A.sub (cnts, 2) * A.sub (cnts, 4) +
              A.sub (cnts, 0) * A.sub (cnts, 3) * A.sub (cnts, 4) +
              A.sub (cnts, 1) * A.sub (cnts, 2) * A.sub (cnts, 3) +
              A.sub (cnts, 1) * A.sub (cnts, 2) * A.sub (cnts, 4) +
              A.sub (cnts, 1) * A.sub (cnts, 3) * A.sub (cnts, 4) +
              A.sub (cnts, 2) * A.sub (cnts, 3) * A.sub (cnts, 4)) ^
           "\n")
  end
