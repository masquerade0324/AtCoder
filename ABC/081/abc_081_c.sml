structure A =  Array
structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun merge ([], ys) = ys
  | merge (xs, []) = xs
  | merge (x::xs, y::ys) = if x >= y then x::merge (xs, y::ys)
                           else y::merge (x::xs, ys)
 
fun split ([], xs, ys)      = (xs, ys)
  | split ([x], xs, ys)     = (x::xs, ys)
  | split (x::y::l, xs, ys) = split (l, x::xs, y::ys)
 
fun msort []  = []
  | msort [x] = [x]
  | msort xs  =
  let
    val (left, right) = split (xs, [], [])
  in
    merge (msort left, msort right)
  end

val () =
  let
    val n = nextInt ()
    val k = nextInt ()
    val ary = A.array (n + 1, 0)
    val i = ref 0
  in
    while !i < n do (
      let
        val ai = nextInt ()
      in
        A.update (ary, ai, A.sub (ary, ai) + 1)
      end;
      i := !i + 1
    );
    print (Int.toString (n - (foldl (op +) 0 (List.take (msort (A.foldl (op ::) [] ary), k)))) ^ "\n")
  end
