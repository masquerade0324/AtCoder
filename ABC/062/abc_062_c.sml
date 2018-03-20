structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun diff (s1, s2, s3) = LI.max (s1, LI.max (s2, s3)) -
                        LI.min (s1, LI.min (s2, s3))

fun solve1 (H, W) : LI.int =
    let
      val h = ref 1
      val s = ref 10000000001
    in
      while !h < H do (
        s := LI.min (!s, diff (!h * W,
                               (H - !h) div 2 * W,
                               (H - !h + 1) div 2 * W));
        s := LI.min (!s, diff (!h * W,
                               (H - !h) * (W div 2),
                               (H - !h) * ((W + 1) div 2)));
        h := !h + 1
      );
      !s
    end

fun solve2 (H, W) : LI.int =
    let
      val w = ref 1
      val s = ref 10000000001
    in
      while !w < W do (
        s := LI.min (!s, diff (!w * H,
                               (W - !w) div 2 * H,
                               (W - !w + 1) div 2 * H));
        s := LI.min (!s, diff (!w * H,
                               (W - !w) * (H div 2),
                               (W - !w) * ((H + 1) div 2)));
        w := !w + 1
      );
      !s
    end

val () =
    let
      val (H, W) = (nextInt (), nextInt ())
    in
      print (LI.toString (LI.min (solve1 (H, W), solve2 (H, W))) ^ "\n")
    end
