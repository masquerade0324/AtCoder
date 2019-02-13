structure A  = Array
structure I  = Int
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO
structure W  = Word

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val N = nextInt ()

val a = A.tabulate (N, fn _ => nextInt ())

val ofst = 101000

val bit = A.array (203000, 0)

fun initBIT () = A.modify (fn _ => 0) bit 

fun sumBIT i =
    let
        val sum = ref 0
        val j = ref (W.fromInt i)
    in
        while W.> (!j, 0w0) do (
            sum := !sum + Array.sub (bit, W.toInt (!j));
            j := W.- (!j, W.andb (!j, ~(!j)))
        );
        !sum
    end

fun addBIT (i, x) =
    let
        val j  = ref (W.fromInt i)
        val j' = ref i
    in
        while W.<= (!j, 0w202000) do (
            j' := W.toInt (!j);
            Array.update (bit, !j', Array.sub (bit, !j') + x);
            j := W.+ (!j, W.andb (!j, ~(!j)))
        )
    end

val NUM = (LI.fromInt N * LI.fromInt (N + 1) div 2 + 1) div 2

(* p(x) : 数列 a の部分列 a[l,r] のうち、x 以上の要素を半数以上含む
 *   <==> sum(l) <= sum(r) を満たす組 (l,r) の個数が半数以上である *)
fun p x =
    let
        val num = ref 0
        val i = ref 0
        val s = ref 0
    in
        initBIT ();
        addBIT (ofst, 1);
        while !i < N do (
            if A.sub (a, !i) >= x then s := !s + 1 else s := !s - 1;
            num := !num + LI.fromInt (sumBIT (!s + ofst));
            addBIT (!s + ofst, 1);
            i := !i + 1
        );
        NUM <= !num
    end

fun solve () =
    let
        val l = ref 0
        val r = ref 1000000007
    in
        while !r - !l > 1 do (
            let
                val m = (!l + !r) div 2
            in
                if p m then l := m else r := m
            end
        );
        !l
    end

val () = print (I.toString (solve ()) ^ "\n")
