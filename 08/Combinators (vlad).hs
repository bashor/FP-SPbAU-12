module Combinators
    ( module Parser
    , many
    , many1
    , char
    , anyChar
    , string
    , digit
    , natural
    , integer
    , spaces
    , try
    , endBy
    , endBy1
    , sepBy
    , sepBy1
    , between
    , brackets
    , parens
    , braces
    , angles
    ) where

import Parser

-- char x парсит символ x, иначе неуспех.
-- execParser (char 'q') "qwerty" == Just ('q', "werty")
-- execParser (char 'x') "qwerty" == Nothing
char :: Char -> Parser Char
char = undefined

-- anyChar парсит любой символ.
-- execParser anyChar "qwerty" == Just ('q', "werty")
-- execParser anyChar "" == Nothing
anyChar :: Parser Char
anyChar = undefined

-- digit парсит одну цифру.
-- execParser digit "qwerty" == Nothing
-- execParser digit "123qwerty" == Just (1, "23qwerty")
-- execParser digit "" == Nothing
digit :: Parser Int
digit = undefined

-- string s парсит строку s, в противном случае неуспех.
-- execParser (string "qwerty") "qwerty" == Just ((), "")
-- execParser (string "qwerty") "qwertyuiop" == Just ((), "uiop")
-- execParser (string "qwerty") "qwerryuiop" == Nothing
-- execParser (string "qwerty") "qwert" == Nothing
string :: String -> Parser ()
string = undefined

-- many p парсит 0 и более раз p.
-- execParser (many (char 'q')) "qwerty" == Just ("q", "werty")
-- execParser (many (char 'q')) "qqqwerty" == Just ("qqq", "werty")
-- execParser (many (char 'q')) "werty" == Just ("", "werty")
-- execParser (many (char 'q')) "" == Just ("", "")
many :: Parser a -> Parser [a]
many p = many1 p `orP` empty []

-- many1 p парсит 0 и более раз p.
-- execParser (many1 (char 'q')) "qwerty" == Just ("q", "werty")
-- execParser (many1 (char 'q')) "qqqwerty" == Just ("qqq", "werty")
-- execParser (many1 (char 'q')) "werty" == Nothing
-- execParser (many1 (char 'q')) "" == Nothing
many1 :: Parser a -> Parser [a]
many1 p = fmap (\(x, xs) -> x:xs) (p `seqP` many p)

-- natural парсит целое положительное число.
-- execParser natural "qwerty" == Nothing
-- execParser natural "123qwerty" == Just (123, "qwerty")
-- execParser natural "-123qwerty" == Nothing
-- execParser natural "" == Nothing
natural :: Parser Integer
natural = undefined

-- integer парсит целое число.
-- execParser integer "qwerty" == Nothing
-- execParser integer "123qwerty" == Just (123, "qwerty")
-- execParser integer "-123qwerty" == Just (-123, "qwerty")
-- execParser integer "-qwerty" == Nothing
integer :: Parser Integer
integer = natural `orP` fmap (\(_,r) -> -r) (char '-' `seqP` natural)

-- spaces парсит последовательность пробелов.
-- execParser spaces "qwerty" == Just ((), "qwerty")
-- execParser spaces "    qwerty" == Just ((), "qwerty")
-- execParser spaces "" == Just ((), "")
spaces :: Parser ()
spaces = undefined

-- try p парсит p и возвращает Just его результат, если p завершается неуспехом, то парсит пустую строку и возвращает Nothing.
-- execParser (try natural) "123qwerty" == Just (Just 123, "qwerty")
-- execParser (try natural) "qwerty" == Just (Nothing, "qwerty")
-- execParser (try (char 'q')) "qwerty" == Just (Just 'q', "werty")
-- execParser (try (char 'x')) "qwerty" == Just (Nothing, "qwerty")
-- execParser (try (char 'x')) "" == Just (Nothing, "")
-- execParser (try eof) "qwerty" == Just (Nothing, "qwerty")
-- execParser (try eof) "" == Just (Just (), "")
try :: Parser a -> Parser (Maybe a)
try = undefined

-- endBy p q парсит (возможно пустую) последовательность p и q.
-- execParser (natural `endBy` char ';') "1;2;3;456;xyz;" == Just ([1,2,3,456], "xyz;")
-- execParser (natural `endBy` char ';') "1;2;3;456" == Just ([1,2,3], "456")
-- execParser (natural `endBy` spaces) "12 25   300" == Just ([12,25,300], "")
-- execParser (natural `endBy` spaces) "qwerty" == Just ([], "qwerty")
-- execParser (natural `endBy` spaces) "" == Just ([], "")
endBy :: Parser a -> Parser b -> Parser [a]
endBy = undefined

-- endBy1 p q парсит (непустую) последовательность p и q.
-- execParser (natural `endBy1` char ';') "1;2;3;456;xyz;" == Just ([1,2,3,456], "xyz;")
-- execParser (natural `endBy1` char ';') "1;2;3;456" == Just ([1,2,3], "456")
-- execParser (natural `endBy1` spaces) "12 25   300" == Just ([12,25,300], "")
-- execParser (natural `endBy1` spaces) "qwerty" == Nothing
-- execParser (natural `endBy1` spaces) "" == Nothing
endBy1 :: Parser a -> Parser b -> Parser [a]
endBy1 = undefined

-- sepBy p q парсит (возможно пустую) последовательность p, разделенных q.
-- execParser (natural `sepBy` char ';') "1;2;3;456;xyz;" == Just ([1,2,3,456], ";xyz;")
-- execParser (natural `sepBy` char ';') "1;2;3;456" == Just ([1,2,3,456], "")
-- execParser (natural `sepBy` spaces) "12 25   300" == Just ([12,25,300], "")
-- execParser (natural `sepBy` spaces) "qwerty" == Just ([], "qwerty")
-- execParser (natural `sepBy` spaces) "" == Just ([], "")
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p q = sepBy1 p q `orP` empty []

-- sepBy1 p q парсит (непустую) последовательность p, разделенных q.
-- execParser (natural `sepBy1` char ';') "1;2;3;456;xyz;" == Just ([1,2,3,456], ";xyz;")
-- execParser (natural `sepBy1` char ';') "1;2;3;456" == Just ([1,2,3,456], "")
-- execParser (natural `sepBy1` spaces) "12 25   300" == Just ([12,25,300], "")
-- execParser (natural `sepBy1` spaces) "qwerty" == Nothing
-- execParser (natural `sepBy1` spaces) "" == Nothing
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p q = fmap (\(x, xs) -> x:xs) $ p `seqP` (fmap snd (q `seqP` sepBy1 p q) `orP` empty [])

-- between a b c парсит a, потом c, потом b, возвращает результат c.
-- execParser (between (char 'a') (char 'b') (char 'c')) "abc" == Nothing
-- execParser (between (char 'a') (char 'b') (char 'c')) "acb" == Just ('c', "")
between :: Parser a -> Parser b -> Parser c -> Parser c
between = undefined

-- brackets p парсит p в квадратных скобках.
-- execParser (brackets (string "qwerty")) "[qwerty]uiop" == Just ((), "uiop")
-- execParser (brackets (string "qwerty")) "[qwertyu]iop" == Nothing
brackets :: Parser a -> Parser a
brackets = undefined

-- parens p парсит p в круглых скобках.
-- execParser (parens spaces) "(   )qwerty" == Just ((), "qwerty")
-- execParser (parens spaces) "(q)werty" == Nothing
parens :: Parser a -> Parser a
parens = undefined

-- braces p парсит p в фигурных скобках.
-- execParser (braces natural) "{123}" == Just (123, "")
-- execParser (braces natural) "{}" == Nothing
braces :: Parser a -> Parser a
braces = undefined

-- angles p парсит p в угловых скобках.
-- execParser (angles digit) "<1>" == Just (1, "")
-- execParser (angles digit) "<1 >" == Nothing
angles :: Parser a -> Parser a
angles = undefined
