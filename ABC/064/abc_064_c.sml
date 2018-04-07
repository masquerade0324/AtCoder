structure A  = Array
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun inc (ary, i) = A.update (ary, i, A.sub (ary, i) + 1)

fun input cnts 0 = ()
  | input cnts n =
    let
      val a = nextInt ()
    in
      if a >= 3200 then inc (cnts, 8) else inc (cnts, a div 400);
      input cnts (n - 1)
    end

fun calColorNum cnts =
    let
      val i   = ref 0
      val num = ref 0
    in
      while !i <= 7 do (
        if A.sub (cnts, !i) > 0 then num := !num + 1 else ();
        i := !i + 1
      );
      !num
    end

val () =
    let
      val cnts = A.array (9, 0)
      val n    = nextInt ()
      val _    = input cnts n
      val num  = calColorNum cnts
      val min  = if num = 0 andalso A.sub (cnts, 8) > 0 then 1 else num
      val max  = num + A.sub (cnts, 8)
    in
      print (Int.toString min ^ " " ^ Int.toString max ^ "\n")
    end
