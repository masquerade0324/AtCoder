structure CV = CharVector
structure I  = Int
structure LI = LargeInt
structure SC = StringCvt
structure T  = TextIO

fun nextLInt () = valOf (T.scanStream (LI.scan SC.DEC) T.stdIn)

fun cnt753 (s, n) =
    if valOf (LI.fromString s) > n then 0
    else
        let 
            val tmp = if CV.exists (fn c => c = #"7") s andalso
                         CV.exists (fn c => c = #"5") s andalso
                         CV.exists (fn c => c = #"3") s
                      then 1 else 0
        in
            cnt753 (s ^ "7", n) + cnt753 (s ^ "5", n) + cnt753 (s ^ "3", n) + tmp 
        end

val () =
    let
        val n = nextLInt ()
   in
       print (I.toString (cnt753 ("7", n) + cnt753 ("5", n) + cnt753 ("3", n)) ^ "\n")
    end
