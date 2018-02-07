structure A  = Array
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun updateWater water (a, b, f) =
  let
    val i = ref 0
    val j = ref 0
  in
    while !i <= 30 do (
      j := 0;
      while !j <= 15 do (
        let val x = 100 * a * !i + 100 * b * !j
        in  if x <= f then A.update (water, x, true) else ()
        end;
        j := !j + 1
      );
      i := !i + 1
    )
  end

fun updateSugar sugar (c, d, f) =
  let
    val i = ref 0
    val j = ref 0
  in
    while !i <= 3000 do (
      j := 0;
      while !j <= 1500 do (
        let val y = c * !i + d * !j
        in  if y <= f then A.update (sugar, y, true) else ()
        end;
        j := !j + 1
      );
      i := !i + 1
    )
  end

fun solve (water, sugar) (a, e, f) =
  let
    val x = ref 0
    val y = ref 0
    val resW = ref (100 * a)
    val resS = ref 0
  in
    while !x <= f do (
      if A.sub (water, !x) then (
        y := 0;
        while !y <= f do (
          if A.sub (sugar, !y) then
            if !x + !y <= f andalso !y * (100 + e) <= e * (!x + !y) then
              if !y * (!resW + !resS) > !resS * (!x + !y) then
                (resW := !x; resS := !y)
              else ()
            else ()
          else ();
          y := !y + 1
        )
      )
      else ();
      x := !x + 1
    );
    print (Int.toString (!resW + !resS) ^ " " ^ Int.toString (!resS) ^ "\n")
  end

val () =
  let
    val (a, b, c, d, e, f) =
      (nextInt (), nextInt (), nextInt (), nextInt (), nextInt (), nextInt ())
    val water = A.array (f + 1, false)
    val sugar = A.array (f + 1, false)
  in
    updateWater water (a, b, f);
    updateSugar sugar (c, d, f);
    solve (water, sugar) (a, e, f)
  end
