structure C = Char
structure S = String
structure T = TextIO

fun isShiritori [] = true
  | isShiritori [s] = true
  | isShiritori (s1::s2::ss) =
  S.sub (s1, S.size s1 - 1) = S.sub (s2, 0) andalso isShiritori (s2::ss)

val () =
  let
    val ss = S.tokens C.isSpace (valOf (TextIO.inputLine T.stdIn))
  in
    print (if isShiritori ss then "YES\n" else "NO\n")
  end
