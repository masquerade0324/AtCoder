structure A  = Array
structure T  = TextIO
structure SC = StringCvt

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

val () =
  let
    val n = nextInt ()
    val cnts = A.array (n + 1, 0)
    val i = ref 1
    val res = ref 0
  in
    while (!i <= n) do (
      let
        val x = nextInt ()
      in
        if x > n then res := !res + 1
        else A.update (cnts, x, A.sub (cnts, x) + 1)
      end;
      i := !i + 1
    );
    i := 1;
    while (!i <= n) do (
      let
        val cnt = A.sub (cnts, !i)
      in
        if cnt = 0 orelse cnt = !i then ()
        else if cnt > !i then res := !res + cnt - !i
        else res := !res + cnt
      end;
      i := !i + 1
    );
    print (Int.toString (!res) ^ "\n")
  end
