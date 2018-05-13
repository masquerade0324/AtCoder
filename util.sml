(* 実行時間を測定する *)
fun mesureRunTime f x =
    let
      val timer    = Timer.startCPUTimer ()
      val result   = f x
      val time     = Timer.checkCPUTimer timer
      val userTime = Time.toMicroseconds (#usr time)
    in
      (LargeInt.toInt userTime, result)
    end

(* 乱数リストを生成する *)
fun mkRandList n (min, max) =
    let
      val seed = Random.rand (0, 1)
    in
      List.tabulate (n, fn _ => Random.randRange (min, max) seed)
    end
