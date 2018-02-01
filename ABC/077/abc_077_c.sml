structure A  = Array
structure L  = List
structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun merge _ ([], ys) = ys
  | merge _ (xs, []) = xs
  | merge f (x::xs, y::ys) = if f (x, y) then x::merge f (xs, y::ys)
                             else y::merge f (x::xs, ys)
 
fun split ([], xs, ys)      = (xs, ys)
  | split ([x], xs, ys)     = (x::xs, ys)
  | split (x::y::l, xs, ys) = split (l, x::xs, y::ys)
 
fun msort _ []  = []
  | msort _ [x] = [x]
  | msort f xs  =
  let
    val (left, right) = split (xs, [], [])
  in
    merge f (msort f left, msort f right)
  end

fun lbound (ary, p) =
  let
    fun lbound' (l, u) =
      let
        val m = l + (u - l) div 2
      in
        (* search range: (l, u] *)
        if u - l <= 1 then u
        else
          if p (A.sub (ary, m)) then lbound' (l, m) else lbound' (m, u)
      end
  in
    lbound' (~1, A.length ary)
  end

val () =
  let
    val n    = nextInt ()
    val aryA = A.fromList (msort (op<) (L.tabulate (n, fn _ => nextInt ())))
    val aryB = A.fromList (msort (op<) (L.tabulate (n, fn _ => nextInt ())))
    val aryC = A.fromList (msort (op>) (L.tabulate (n, fn _ => nextInt ())))
    val j    = ref 0
    val sum  = ref 0
  in
    while !j < n do (
      let
        val x = Int.toLarge (lbound (aryA, fn ai => ai >= A.sub (aryB, !j)))
        val y = Int.toLarge (lbound (aryC, fn ci => ci <= A.sub (aryB, !j)))
      in
        sum := !sum + x * y;
        j := !j + 1
      end);
    print (LargeInt.toString (!sum) ^ "\n")
  end
