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
import Control.Applicative hiding (empty, many)

-- parse boolP "Trueqwerty" == Just (True, "qwerty")
-- parse boolP "False" == Just (False, "")
-- parse boolP "qwerty" == Nothing
boolP :: Parser Bool
boolP = (\_ -> True) <$> string "True" `orP` (\_ -> False) <$> string "False"

-- parse (maybeP natural) "Nothingqwerty" == Just (Nothing, "qwerty")
-- parse (maybeP natural) "Just 123qwerty" == Just (Just 123, "qwerty")
-- parse (maybeP natural) "Just123qwerty" == Nothing
maybeP :: Parser a -> Parser (Maybe a)
maybeP p = (\_ -> Nothing) <$> string "Nothing" `orP` (\(_, x) -> Just x) <$> (string "Just" `seqP` char ' ' `seqP` p)

-- runParser (listP integer) "[1,-23,25,347]" == Just [1,-23,25,347]
-- runParser (listP integer) "[1 ,  -23,  25   ,347]" == Nothing
listP :: Parser a -> Parser [a]
listP p = brackets (p `sepBy` char ',')

-- runParser (listP' integer) "[1,-23,25,347]" == Just [1,-23,25,347]
-- runParser (listP' integer) "[1 ,  -23,  25   ,347]" == Just [1,-23,25,347]
listP' :: Parser a -> Parser [a]
listP' p = brackets (p `sepBy` (between spaces spaces (char ',')))

main = void $ runTestTT $ test
    $    label "empty"
    [ parse (empty 4) "qwerty" ~?= Just (4, "qwerty")
    , parse (empty 'x') "" ~?= Just ('x', "")
    ] ++ label "failP"
    [ parse (failP :: Parser ()) "qwerty" ~?= Nothing
    , parse (failP :: Parser ()) "" ~?= Nothing
    ] ++ label "satisfy"
    [ parse (satisfy (/= 'x')) "qwerty" ~?= Just ('q', "werty")
    , parse (satisfy (/= 'x')) "xwerty" ~?= Nothing
    ] ++ label "seqP"
    [ parse (satisfy (== 'q') `seqP` satisfy (== 'w')) "qwerty" ~?= Just (('q', 'w'), "erty")
    , parse (satisfy (== 'q') `seqP` satisfy (== 'x')) "qwerty" ~?= Nothing
    , parse (satisfy (== 'x') `seqP` satisfy (== 'w')) "qwerty" ~?= Nothing
    ] ++ label "orP"
    [ parse (satisfy (== 'q') `orP` satisfy (== 'x')) "qwerty" ~?= Just ('q', "werty")
    , parse (satisfy (== 'x') `orP` satisfy (== 'q')) "qwerty" ~?= Just ('q', "werty")
    , parse (satisfy (== 'x') `orP` satisfy (== 'y')) "qwerty" ~?= Nothing
    ] ++ label "eof"
    [ parse eof "qwerty" ~?= Nothing
    , parse eof "" ~?= Just ((), "")
    ] ++ label "char"
    [ parse (char 'q') "qwerty" ~?= Just ('q', "werty")
    , parse (char 'x') "qwerty" ~?= Nothing
    ] ++ label "anyChar"
    [ parse anyChar "qwerty" ~?= Just ('q', "werty")
    , parse anyChar "" ~?= Nothing
    ] ++ label "digit"
    [ parse digit "qwerty" ~?= Nothing
    , parse digit "123qwerty" ~?= Just (1, "23qwerty")
    , parse digit "" ~?= Nothing
    ] ++ label "string"
    [ parse (string "qwerty") "qwerty" ~?= Just ((), "")
    , parse (string "qwerty") "qwertyuiop" ~?= Just ((), "uiop")
    , parse (string "qwerty") "qwerryuiop" ~?= Nothing
    , parse (string "qwerty") "qwert" ~?= Nothing
    ] ++ label "many"
    [ parse (many (char 'q')) "qwerty" ~?= Just ("q", "werty")
    , parse (many (char 'q')) "qqqwerty" ~?= Just ("qqq", "werty")
    , parse (many (char 'q')) "werty" ~?= Just ("", "werty")
    , parse (many (char 'q')) "" ~?= Just ("", "")
    ] ++ label "many1"
    [ parse (many1 (char 'q')) "qwerty" ~?= Just ("q", "werty")
    , parse (many1 (char 'q')) "qqqwerty" ~?= Just ("qqq", "werty")
    , parse (many1 (char 'q')) "werty" ~?= Nothing
    , parse (many1 (char 'q')) "" ~?= Nothing
    ] ++ label "natural"
    [ parse natural "qwerty" ~?= Nothing
    , parse natural "123qwerty" ~?= Just (123, "qwerty")
    , parse natural "-123qwerty" ~?= Nothing
    , parse natural "" ~?= Nothing
    ] ++ label "integer"
    [ parse integer "qwerty" ~?= Nothing
    , parse integer "123qwerty" ~?= Just (123, "qwerty")
    , parse integer "-123qwerty" ~?= Just (-123, "qwerty")
    , parse integer "-qwerty" ~?= Nothing
    ] ++ label "spaces"
    [ parse spaces "qwerty" ~?= Just ((), "qwerty")
    , parse spaces "    qwerty" ~?= Just ((), "qwerty")
    , parse spaces "" ~?= Just ((), "")
    ] ++ label "try"
    [ parse (try natural) "123qwerty" ~?= Just (Just 123, "qwerty")
    , parse (try natural) "qwerty" ~?= Just (Nothing, "qwerty")
    , parse (try (char 'q')) "qwerty" ~?= Just (Just 'q', "werty")
    , parse (try (char 'x')) "qwerty" ~?= Just (Nothing, "qwerty")
    , parse (try (char 'x')) "" ~?= Just (Nothing, "")
    , parse (try eof) "qwerty" ~?= Just (Nothing, "qwerty")
    , parse (try eof) "" ~?= Just (Just (), "")
    ]  ++ label "endBy"
    [ parse (natural `endBy` char ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], "xyz;")
    , parse (natural `endBy` char ';') "1;2;3;456" ~?= Just ([1,2,3], "456")
    , parse (natural `endBy` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , parse (natural `endBy` spaces) "qwerty" ~?= Just ([], "qwerty")
    , parse (natural `endBy` spaces) "" ~?= Just ([], "")
    ] ++ label "endBy1"
    [ parse (natural `endBy1` char ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], "xyz;")
    , parse (natural `endBy1` char ';') "1;2;3;456" ~?= Just ([1,2,3], "456")
    , parse (natural `endBy1` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , parse (natural `endBy1` spaces) "qwerty" ~?= Nothing
    , parse (natural `endBy1` spaces) "" ~?= Nothing
    ] ++ label "sepBy"
    [ parse (natural `sepBy` char ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], ";xyz;")
    , parse (natural `sepBy` char ';') "1;2;3;456" ~?= Just ([1,2,3,456], "")
    , parse (natural `sepBy` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , parse (natural `sepBy` spaces) "qwerty" ~?= Just ([], "qwerty")
    , parse (natural `sepBy` spaces) "" ~?= Just ([], "")
    ] ++ label "sepBy1"
    [ parse (natural `sepBy1` char ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], ";xyz;")
    , parse (natural `sepBy1` char ';') "1;2;3;456" ~?= Just ([1,2,3,456], "")
    , parse (natural `sepBy1` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , parse (natural `sepBy1` spaces) "qwerty" ~?= Nothing
    , parse (natural `sepBy1` spaces) "" ~?= Nothing
    ] ++ label "between"
    [ parse (between (char 'a') (char 'b') (char 'c')) "abc" ~?= Nothing
    , parse (between (char 'a') (char 'b') (char 'c')) "acb" ~?= Just ('c', "")
    ] ++ label "brackets"
    [ parse (brackets (string "qwerty")) "[qwerty]uiop" ~?= Just ((), "uiop")
    , parse (brackets (string "qwerty")) "[qwertyu]iop" ~?= Nothing
    ] ++ label "parens"
    [ parse (parens spaces) "(   )qwerty" ~?= Just ((), "qwerty")
    , parse (parens spaces) "(q)werty" ~?= Nothing
    ] ++ label "braces"
    [ parse (braces natural) "{123}" ~?= Just (123, "")
    , parse (braces natural) "{}" ~?= Nothing
    ] ++ label "angles"
    [ parse (angles digit) "<1>" ~?= Just (1, "")
    , parse (angles digit) "<1 >" ~?= Nothing
    ] ++ label "boolP"
    [ parse boolP "Trueqwerty" ~?= Just (True, "qwerty")
    , parse boolP "False" ~?= Just (False, "")
    , parse boolP "qwerty" ~?= Nothing
    ] ++ label "maybeP"
    [ parse (maybeP natural) "Nothingqwerty" ~?= Just (Nothing, "qwerty")
    , parse (maybeP natural) "Just 123qwerty" ~?= Just (Just 123, "qwerty")
    , parse (maybeP natural) "Just123qwerty" ~?= Nothing
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
