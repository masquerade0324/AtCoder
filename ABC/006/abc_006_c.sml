structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun printTriple (x, y, z) = print (I.toString x ^ " " ^
                                   I.toString y ^ " " ^
                                   I.toString z ^ "\n")

val () =
    let
        val (N, M) = (nextInt (), nextInt ())
        val zs   = L.tabulate (N + 1, fn z => z)
        val yzs  = map (fn z => (M - 2 * N - 2 * z, z)) zs
        val xyzs = map (fn (y, z) => (N - y - z, y, z)) yzs

        fun solve []             = (~1, ~1, ~1)
          | solve ((x, y, z)::l) = if x >= 0 andalso y >= 0 then (x, y, z)
                                   else solve l
    in
        case solve xyzs of
            (~1, ~1, ~1) => print "-1 -1 -1\n"
          | xyz          => printTriple xyz
    end
