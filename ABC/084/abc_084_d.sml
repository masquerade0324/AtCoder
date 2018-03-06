structure A  = Array
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (Int.scan SC.DEC) T.stdIn)

fun eratos n primes =
  let
    fun sieve i j =
      if j <= n then (A.update (primes, j, false); sieve i (j + i))
      else ()
    fun loop i =
      if i * i <= n
      then if A.sub (primes, i) then (sieve i (i * 2); loop (i + 2))
           else loop (i + 2)
      else ()
  in
    sieve 2 4;
    loop 3
  end

fun calCsum n primes csum =
  let
    val i = ref 1
  in
    A.update (csum, 0, if A.sub (primes, 0) then 1 else 0);
    while !i <= n do (
      A.update (csum, !i, if A.sub (primes, !i) andalso
                             A.sub (primes, ((!i + 1) div 2))
                          then A.sub (csum, !i - 1) + 1
                          else A.sub (csum, !i - 1));
      i := !i + 1
    )
  end

fun solve csum q =
  if q <= 0 then ()
  else
    let
      val (l, r) = (nextInt (), nextInt ())
    in
      print (Int.toString (A.sub (csum, r) - A.sub (csum, l - 1)) ^ "\n");
      solve csum (q - 1)
    end

val () =
  let
    val n = 100000
    val primes = A.array (n + 1, true)
    val _ = (A.update (primes, 0, false); A.update (primes, 1, false))
    val csum = A.array (n + 1, 0)
    val q = nextInt ()
  in
    eratos n primes;
    calCsum n primes csum;
    solve csum q
  end
