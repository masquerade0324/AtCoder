structure I  = Int
structure SC = StringCvt
structure T  = TextIO

fun nextInt () = valOf (T.scanStream (I.scan SC.DEC) T.stdIn)

val () =
    let
        val n = nextInt ()
    in
        if n <= 111 then print "111\n"
        else if n <= 222 then print "222\n"
        else if n <= 333 then print "333\n"
        else if n <= 444 then print "444\n"
        else if n <= 555 then print "555\n"
        else if n <= 666 then print "666\n"
        else if n <= 777 then print "777\n"
        else if n <= 888 then print "888\n"
        else print "999\n"
    end
