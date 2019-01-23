structure A  = Array
structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

datatype color = R | B

datatype tree = E | T of color * tree * int * tree

val empty = E

fun member (x, E)              = false
  | member (x, T (_, a, y, b)) = if x < y then member (x, a)
                                 else if x > y then member (x ,b)
                                 else true

fun size E                = 0
  | size (T (_, a, x, b)) = 1 + size a + size b

fun balance (B, T (R, T (R, a, x, b), y, c), z, d) =
    T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (B, T (R, a, x, T (R, b, y, c)), z, d) =
    T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (B, a , x,T (R, T (R, b, y, c), z, d)) =
    T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (B, a, x, T (R, b, y, T (R, c, z, d))) =
    T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance body = T body

fun insert (x, s) =
    let
        fun ins E = T (R, E, x, E)
          | ins (s as T (color, a, y, b)) =
            if x < y then balance (color, ins a, y, b)
            else if x > y then balance (color, a, y, ins b)
            else s

        val T (_, a, y, b) = ins s
    in
        T (B, a, y, b)
    end

fun toMinOdd x = if x mod 2 = 0 then toMinOdd (x div 2) else x

val () =
    let
        val N = nextInt ()
        val l = L.tabulate (N, fn _ => nextInt ())
        val set = foldl (fn (x, s) => insert (toMinOdd x, s)) E l
    in
        print (I.toString (size set) ^ "\n")
    end
