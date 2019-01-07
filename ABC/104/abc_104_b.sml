structure C  = Char
structure CV = CharVector
structure S  = String
structure SC = StringCvt
structure T  = TextIO

fun scan rdr strm = SOME (SC.splitl (not o C.isSpace) rdr (SC.skipWS rdr strm))

fun next () = valOf (T.scanStream scan T.stdIn)

val () =
    let
        val s = next ()
    in
        if S.sub (s, 0) = #"A" then
            let
                val s'  = substring (s, 2, size s - 3)
                val cnt = CV.foldl (fn (c, i) => if c = #"C" then i + 1 else i) 0 s'
            in
                if cnt = 1 then
                    if C.isLower (S.sub (s, 1)) andalso
                       C.isLower (S.sub (s, size s - 1)) andalso
                       CV.all C.isLower (CV.map (fn #"C" => #"c" | c => c) s')
                    then
                        print "AC\n"
                    else
                        print "WA\n"
                else
                    print "WA\n"
            end
        else
            print "WA\n"
    end
