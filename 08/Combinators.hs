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
import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative hiding (empty, many)

-- char x парсит символ x, иначе неуспех.
-- parse (char 'q') "qwerty" == Just ('q', "werty")
-- parse (char 'x') "qwerty" == Nothing
char :: Char -> Parser Char
char x = satisfy (== x)

-- anyChar парсит любой символ.
-- parse anyChar "qwerty" == Just ('q', "werty")
-- parse anyChar "" == Nothing
anyChar :: Parser Char
anyChar = satisfy (\c -> True)

-- digit парсит одну цифру.
-- parse digit "qwerty" == Nothing
-- parse digit "123qwerty" == Just (1, "23qwerty")
-- parse digit "" == Nothing

digit' :: Parser Char
digit' = satisfy isDigit

digit :: Parser Int
digit = fmap digitToInt digit'


-- string s парсит строку s, в противном случае неуспех.
-- parse (string "qwerty") "qwerty" == Just ((), "")
-- parse (string "qwerty") "qwertyuiop" == Just ((), "uiop")
-- parse (string "qwerty") "qwerryuiop" == Nothing
-- parse (string "qwerty") "qwert" == Nothing
string :: String -> Parser ()
string "" = empty ()
string (x:xs) = fmap snd (char x `seqP` string xs)

-- many p парсит 0 и более раз p.
-- parse (many (char 'q')) "qwerty" == Just ("q", "werty")
-- parse (many (char 'q')) "qqqwerty" == Just ("qqq", "werty")
-- parse (many (char 'q')) "werty" == Just ("", "werty")
-- parse (many (char 'q')) "" == Just ("", "")
many :: Parser a -> Parser [a]
many p = many1 p `orP` empty []

-- many1 p парсит 1 и более раз p.
-- parse (many1 (char 'q')) "qwerty" == Just ("q", "werty")
-- parse (many1 (char 'q')) "qqqwerty" == Just ("qqq", "werty")
-- parse (many1 (char 'q')) "werty" == Nothing
-- parse (many1 (char 'q')) "" == Nothing
many1 :: Parser a -> Parser [a]
many1 p = fmap (\(x, xs) -> x:xs) (p `seqP` many p)

-- natural парсит целое положительное число.
-- parse natural "qwerty" == Nothing
-- parse natural "123qwerty" == Just (123, "qwerty")
-- parse natural "-123qwerty" -- == Nothing    
-- parse natural "" == Nothing
natural :: Parser Integer
natural = fmap read (many1 digit')

-- integer парсит целое число.
-- parse integer "qwerty" == Nothing
-- parse integer "123qwerty" == Just (123, "qwerty")
-- parse integer "-123qwerty" == Just (-123, "qwerty")
-- parse integer "-qwerty" == Nothing
integer :: Parser Integer
integer = natural `orP` fmap (\(_,r) -> -r) (char '-' `seqP` natural)

-- spaces парсит последовательность пробелов.
-- parse spaces "qwerty" == Just ((), "qwerty")
-- parse spaces "    qwerty" == Just ((), "qwerty")
-- parse spaces "" == Just ((), "")
spaces :: Parser ()
spaces = fmap (\s -> ()) (many $ char ' ')

-- try p парсит p и возвращает Just его результат, если p завершается неуспехом, то парсит пустую строку и возвращает Nothing.
-- parse (try natural) "123qwerty" == Just (Just 123, "qwerty")
-- parse (try natural) "qwerty" == Just (Nothing, "qwerty")
-- parse (try (char 'q')) "qwerty" == Just (Just 'q', "werty")
-- parse (try (char 'x')) "qwerty" == Just (Nothing, "qwerty")
-- parse (try (char 'x')) "" == Just (Nothing, "")
-- parse (try eof) "qwerty" == Just (Nothing, "qwerty")
-- parse (try eof) "" == Just (Just (), "")
try :: Parser a -> Parser (Maybe a)
try p = fmap Just p `orP` empty Nothing

-- endBy p q парсит (возможно пустую) последовательность p и q.
-- parse (natural `endBy` char ';') "1;2;3;456;xyz;" == Just ([1,2,3,456], "xyz;")
-- parse (natural `endBy` char ';') "1;2;3;456" == Just ([1,2,3], "456")
-- parse (natural `endBy` spaces) "12 25   300" == Just ([12,25,300], "")
-- parse (natural `endBy` spaces) "qwerty" == Just ([], "qwerty")
-- parse (natural `endBy` spaces) "" == Just ([], "")
endBy :: Parser a -> Parser b -> Parser [a]
endBy p1 p2 = many $ fst <$> (p1 `seqP` p2)
-- endBy p1 p2 = catMaybes <$> many (fmap Just p1 `orP` fmap (\_->Nothing) p2)

-- endBy1 p q парсит (непустую) последовательность p и q.
-- parse (natural `endBy1` char ';') "1;2;3;456;xyz;" == Just ([1,2,3,456], "xyz;")
-- parse (natural `endBy1` char ';') "1;2;3;456" == Just ([1,2,3], "456")
-- parse (natural `endBy1` spaces) "12 25   300" == Just ([12,25,300], "")
-- parse (natural `endBy1` spaces) "qwerty" == Nothing
-- parse (natural `endBy1` spaces) "" == Nothing
endBy1 :: Parser a -> Parser b -> Parser [a]
endBy1 p1 p2 = many1 $ fst <$> (p1 `seqP` p2)

-- sepBy p q парсит (возможно пустую) последовательность p, разделенных q.
-- parse (natural `sepBy` char ';') "1;2;3;456;xyz;" == Just ([1,2,3,456], ";xyz;")
-- parse (natural `sepBy` char ';') "1;2;3;456" == Just ([1,2,3,456], "")
-- parse (natural `sepBy` spaces) "12 25   300" == Just ([12,25,300], "")
-- parse (natural `sepBy` spaces) "qwerty" == Just ([], "qwerty")
-- parse (natural `sepBy` spaces) "" == Just ([], "")
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p q = sepBy1 p q `orP` empty []

-- sepBy1 p q парсит (непустую) последовательность p, разделенных q.
-- parse (natural `sepBy1` char ';') "1;2;3;456;xyz;" == Just ([1,2,3,456], ";xyz;")
-- parse (natural `sepBy1` char ';') "1;2;3;456" == Just ([1,2,3,456], "")
-- parse (natural `sepBy1` spaces) "12 25   300" == Just ([12,25,300], "")
-- parse (natural `sepBy1` spaces) "qwerty" == Nothing
-- parse (natural `sepBy1` spaces) "" == Nothing
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p q = fmap (\(x, xs) -> x:xs) $ p `seqP` (fmap snd (q `seqP` sepBy1 p q) `orP` empty [])

-- between a b c парсит a, потом c, потом b, возвращает результат c.
-- parse (between (char 'a') (char 'b') (char 'c')) "abc" == Nothing
-- parse (between (char 'a') (char 'b') (char 'c')) "acb" == Just ('c', "")
between :: Parser a -> Parser b -> Parser c -> Parser c
between pa pb pc = fmap (snd.fst) (pa `seqP` pc `seqP` pb)

-- brackets p парсит p в квадратных скобках.
-- parse (brackets (string "qwerty")) "[qwerty]uiop" == Just ((), "uiop")
-- parse (brackets (string "qwerty")) "[qwertyu]iop" == Nothing
brackets :: Parser a -> Parser a
brackets p = between (char '[') (char ']') p

-- parens p парсит p в круглых скобках.
-- parse (parens spaces) "(   )qwerty" == Just ((), "qwerty")
-- parse (parens spaces) "(q)werty" == Nothing
parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') p

-- braces p парсит p в фигурных скобках.
-- parse (braces natural) "{123}" == Just (123, "")
-- parse (braces natural) "{}" == Nothing
braces :: Parser a -> Parser a
braces p = between (char '{') (char '}') p

-- angles p парсит p в угловых скобках.
-- parse (angles digit) "<1>" == Just (1, "")
-- parse (angles digit) "<1 >" == Nothing
angles :: Parser a -> Parser a
angles p = between (char '<') (char '>') p


