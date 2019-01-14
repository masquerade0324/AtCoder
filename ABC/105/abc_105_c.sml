structure I  = Int
structure L  = List
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

fun divModM2 m = if m mod ~2 = ~1 then (m div ~2 + 1 , 1) else (m div ~2, 0)

fun toBaseM2 m =
    let
        val (q, r) = divModM2 m
    in
        if q = 0 then [I.toString r]
        else I.toString r :: toBaseM2 q
    end

val () =
    let
        val N = nextInt ()
    in
        print ((concat o rev o toBaseM2) N ^ "\n")
    end
