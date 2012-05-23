module Main
    ( main
    , boolP
    , maybeP
    , listP
    , listP'
    ) where

import Combinators
import Control.Monad
import Test.HUnit

-- execParser boolP "Trueqwerty" == Just (True, "qwerty")
-- execParser boolP "False" == Just (False, "")
-- execParser boolP "qwerty" == Nothing
boolP :: Parser Bool
boolP = undefined

-- execParser (maybeP natural) "Nothingqwerty" == Just (Nothing, "qwerty")
-- execParser (maybeP natural) "Just 123qwerty" == Just (Just 123, "qwerty")
-- execParser (maybeP natural) "Just123qwerty" == Nothing
maybeP :: Parser a -> Parser (Maybe a)
maybeP = undefined

-- runParser (listP integer) "[1,-23,25,347]" == Just [1,-23,25,347]
-- runParser (listP integer) "[1 ,  -23,  25   ,347]" == Nothing
listP :: Parser a -> Parser [a]
listP = undefined

-- runParser (listP' integer) "[1,-23,25,347]" == Just [1,-23,25,347]
-- runParser (listP' integer) "[1 ,  -23,  25   ,347]" == Just [1,-23,25,347]
listP' :: Parser a -> Parser [a]
listP' = undefined

main = void $ runTestTT $ test
    $    label "empty"
    [ execParser (empty 4) "qwerty" ~?= Just (4, "qwerty")
    , execParser (empty 'x') "" ~?= Just ('x', "")
    ] ++ label "failP"
    [ execParser (failP :: Parser ()) "qwerty" ~?= Nothing
    , execParser (failP :: Parser ()) "" ~?= Nothing
    ] ++ label "satisfy"
    [ execParser (satisfy (/= 'x')) "qwerty" ~?= Just ('q', "werty")
    , execParser (satisfy (/= 'x')) "xwerty" ~?= Nothing
    ] ++ label "seqP"
    [ execParser (satisfy (== 'q') `seqP` satisfy (== 'w')) "qwerty" ~?= Just (('q', 'w'), "erty")
    , execParser (satisfy (== 'q') `seqP` satisfy (== 'x')) "qwerty" ~?= Nothing
    , execParser (satisfy (== 'x') `seqP` satisfy (== 'w')) "qwerty" ~?= Nothing
    ] ++ label "orP"
    [ execParser (satisfy (== 'q') `orP` satisfy (== 'x')) "qwerty" ~?= Just ('q', "werty")
    , execParser (satisfy (== 'x') `orP` satisfy (== 'q')) "qwerty" ~?= Just ('q', "werty")
    , execParser (satisfy (== 'x') `orP` satisfy (== 'y')) "qwerty" ~?= Nothing
    ] ++ label "eof"
    [ execParser eof "qwerty" ~?= Nothing
    , execParser eof "" ~?= Just ((), "")
    ] ++ label "char"
    [ execParser (char 'q') "qwerty" ~?= Just ('q', "werty")
    , execParser (char 'x') "qwerty" ~?= Nothing
    ] ++ label "anyChar"
    [ execParser anyChar "qwerty" ~?= Just ('q', "werty")
    , execParser anyChar "" ~?= Nothing
    ] ++ label "digit"
    [ execParser digit "qwerty" ~?= Nothing
    , execParser digit "123qwerty" ~?= Just (1, "23qwerty")
    , execParser digit "" ~?= Nothing
    ] ++ label "string"
    [ execParser (string "qwerty") "qwerty" ~?= Just ((), "")
    , execParser (string "qwerty") "qwertyuiop" ~?= Just ((), "uiop")
    , execParser (string "qwerty") "qwerryuiop" ~?= Nothing
    , execParser (string "qwerty") "qwert" ~?= Nothing
    ] ++ label "many"
    [ execParser (many (char 'q')) "qwerty" ~?= Just ("q", "werty")
    , execParser (many (char 'q')) "qqqwerty" ~?= Just ("qqq", "werty")
    , execParser (many (char 'q')) "werty" ~?= Just ("", "werty")
    , execParser (many (char 'q')) "" ~?= Just ("", "")
    ] ++ label "many1"
    [ execParser (many1 (char 'q')) "qwerty" ~?= Just ("q", "werty")
    , execParser (many1 (char 'q')) "qqqwerty" ~?= Just ("qqq", "werty")
    , execParser (many1 (char 'q')) "werty" ~?= Nothing
    , execParser (many1 (char 'q')) "" ~?= Nothing
    ] ++ label "natural"
    [ execParser natural "qwerty" ~?= Nothing
    , execParser natural "123qwerty" ~?= Just (123, "qwerty")
    , execParser natural "-123qwerty" ~?= Nothing
    , execParser natural "" ~?= Nothing
    ] ++ label "integer"
    [ execParser integer "qwerty" ~?= Nothing
    , execParser integer "123qwerty" ~?= Just (123, "qwerty")
    , execParser integer "-123qwerty" ~?= Just (-123, "qwerty")
    , execParser integer "-qwerty" ~?= Nothing
    ] ++ label "spaces"
    [ execParser spaces "qwerty" ~?= Just ((), "qwerty")
    , execParser spaces "    qwerty" ~?= Just ((), "qwerty")
    , execParser spaces "" ~?= Just ((), "")
    ] ++ label "try"
    [ execParser (try natural) "123qwerty" ~?= Just (Just 123, "qwerty")
    , execParser (try natural) "qwerty" ~?= Just (Nothing, "qwerty")
    , execParser (try (char 'q')) "qwerty" ~?= Just (Just 'q', "werty")
    , execParser (try (char 'x')) "qwerty" ~?= Just (Nothing, "qwerty")
    , execParser (try (char 'x')) "" ~?= Just (Nothing, "")
    , execParser (try eof) "qwerty" ~?= Just (Nothing, "qwerty")
    , execParser (try eof) "" ~?= Just (Just (), "")
    ] ++ label "endBy"
    [ execParser (natural `endBy` char ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], "xyz;")
    , execParser (natural `endBy` char ';') "1;2;3;456" ~?= Just ([1,2,3], "456")
    , execParser (natural `endBy` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , execParser (natural `endBy` spaces) "qwerty" ~?= Just ([], "qwerty")
    , execParser (natural `endBy` spaces) "" ~?= Just ([], "")
    ] ++ label "endBy1"
    [ execParser (natural `endBy1` char ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], "xyz;")
    , execParser (natural `endBy1` char ';') "1;2;3;456" ~?= Just ([1,2,3], "456")
    , execParser (natural `endBy1` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , execParser (natural `endBy1` spaces) "qwerty" ~?= Nothing
    , execParser (natural `endBy1` spaces) "" ~?= Nothing
    ] ++ label "sepBy"
    [ execParser (natural `sepBy` char ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], ";xyz;")
    , execParser (natural `sepBy` char ';') "1;2;3;456" ~?= Just ([1,2,3,456], "")
    , execParser (natural `sepBy` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , execParser (natural `sepBy` spaces) "qwerty" ~?= Just ([], "qwerty")
    , execParser (natural `sepBy` spaces) "" ~?= Just ([], "")
    ] ++ label "sepBy1"
    [ execParser (natural `sepBy1` char ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], ";xyz;")
    , execParser (natural `sepBy1` char ';') "1;2;3;456" ~?= Just ([1,2,3,456], "")
    , execParser (natural `sepBy1` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , execParser (natural `sepBy1` spaces) "qwerty" ~?= Nothing
    , execParser (natural `sepBy1` spaces) "" ~?= Nothing
    ] ++ label "between"
    [ execParser (between (char 'a') (char 'b') (char 'c')) "abc" ~?= Nothing
    , execParser (between (char 'a') (char 'b') (char 'c')) "acb" ~?= Just ('c', "")
    ] ++ label "brackets"
    [ execParser (brackets (string "qwerty")) "[qwerty]uiop" ~?= Just ((), "uiop")
    , execParser (brackets (string "qwerty")) "[qwertyu]iop" ~?= Nothing
    ] ++ label "parens"
    [ execParser (parens spaces) "(   )qwerty" ~?= Just ((), "qwerty")
    , execParser (parens spaces) "(q)werty" ~?= Nothing
    ] ++ label "braces"
    [ execParser (braces natural) "{123}" ~?= Just (123, "")
    , execParser (braces natural) "{}" ~?= Nothing
    ] ++ label "angles"
    [ execParser (angles digit) "<1>" ~?= Just (1, "")
    , execParser (angles digit) "<1 >" ~?= Nothing
    ] ++ label "boolP"
    [ execParser boolP "Trueqwerty" ~?= Just (True, "qwerty")
    , execParser boolP "False" ~?= Just (False, "")
    , execParser boolP "qwerty" ~?= Nothing
    ] ++ label "maybeP"
    [ execParser (maybeP natural) "Nothingqwerty" ~?= Just (Nothing, "qwerty")
    , execParser (maybeP natural) "Just 123qwerty" ~?= Just (Just 123, "qwerty")
    , execParser (maybeP natural) "Just123qwerty" ~?= Nothing
    ] ++ label "listP"
    [ runParser (listP integer) "[1,-23,25,347]" ~?= Just [1,-23,25,347]
    , runParser (listP integer) "[1 ,  -23,  25   ,347]" ~?= Nothing
    ] ++ label "listP'"
    [ runParser (listP' integer) "[1,-23,25,347]" ~?= Just [1,-23,25,347]
    , runParser (listP' integer) "[1 ,  -23,  25   ,347]" ~?= Just [1,-23,25,347]
    ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
