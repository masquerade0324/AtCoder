structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val N = nextInt ()
        val (NG1, NG2, NG3) = (nextInt (), nextInt (), nextInt ())
        val n = ref N
        val i = ref 0
        val break = ref false
    in
        if !n = NG1 orelse !n = NG2 orelse !n = NG3 then print "NO\n"
        else
            (while (!i < 100) andalso not (!break) do (
                 if !n - 3 <> NG1 andalso !n - 3 <> NG2 andalso !n - 3 <> NG3
                 then n := !n - 3
                 else if !n - 2 <> NG1 andalso !n - 2 <> NG2 andalso !n - 2 <> NG3
                 then n := !n - 2
                 else if !n - 1 <> NG1 andalso !n - 1 <> NG2 andalso !n - 1 <> NG3
                 then n := !n - 1
                 else break := true;
                 i := !i + 1
             );
             if !n <= 0 andalso not (!break) then print "YES\n"
             else print "NO\n")
    end
