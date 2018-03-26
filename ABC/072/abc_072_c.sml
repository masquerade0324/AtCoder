structure A  = Array
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1)

fun count cnts n =
    if n <= 0 then ()
    else let val a = nextInt ()
         in  inc (cnts, a);
             inc (cnts, a + 1);
             inc (cnts, a + 2);
             count cnts (n - 1)
         end

val () =
  let
    val cnts = A.array (100002, 0)
    val n    = nextInt ()
  in
    count cnts n;
    print (Int.toString (A.foldl Int.max 0 cnts) ^ "\n")
  end
