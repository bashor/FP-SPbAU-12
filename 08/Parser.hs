module Parser
    ( Parser
    , empty
    , satisfy
    , failP
    , seqP
    , orP
    , eof
    , runParser
    , parse
    ) where

import Control.Arrow
import Data.Maybe

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser g) = Parser h where
        h s = fmap (\(a, s) -> (f a, s)) (g s)

-- runParser p s запускает парсер p на строке s.
runParser :: Parser a -> String -> Maybe a
runParser (Parser h) s = fmap fst (h s)

-- empty x парсит пустую строку и возвращает x.
-- parse (empty 4) "qwerty" == Just (4, "qwerty")
-- parse (empty 'x') "" == Just ('x', "")
empty :: a -> Parser a
empty a = Parser h where h s = Just (a, s) 

-- failP всегда завершается неуспехом.
-- parse failP "qwerty" == Nothing
-- parse failP "" == Nothing
failP :: Parser a
failP = Parser h where h _ = Nothing

-- satisfy p парсит один символ и возвращает его, если он удовлетворяет предикату p. В противном случае неуспех.
-- parse (satisfy (/= 'x')) "qwerty" == Just ('q', "werty")
-- parse (satisfy (/= 'x')) "xwerty" == Nothing
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser h where
    h (c:s) | p c  = Just (c, s)
    h _            = Nothing

infixl 3 `orP`
infixl 4 `seqP`

-- seqP p q парсит последовательно p и q, возвращает их результаты в паре.
-- parse (satisfy (== 'q') `seqP` satisfy (== 'w')) "qwerty" == Just (('q', 'w'), "erty")
-- parse (satisfy (== 'q') `seqP` satisfy (== 'x')) "qwerty" == Nothing
-- parse (satisfy (== 'x') `seqP` satisfy (== 'w')) "qwerty" == Nothing
seqP :: Parser a -> Parser b -> Parser (a, b)
seqP (Parser f) (Parser g) = Parser h where
    h s = if isNothing $ f s then Nothing else 
        let Just (x, s') = f s in fmap (\(y, s'') -> ((x, y), s'')) (g s')

-- orP p q парсит p, если тот завершился неуспехом парсит q.
-- parse (satisfy (== 'q') `orP` satisfy (== 'x')) "qwerty" == Just ('q', "werty")
-- parse (satisfy (== 'x') `orP` satisfy (== 'q')) "qwerty" == Just ('q', "werty")
-- parse (satisfy (== 'x') `orP` satisfy (== 'y')) "qwerty" == Nothing
orP :: Parser a -> Parser a -> Parser a
orP (Parser f) (Parser g) = Parser h where
    h s = if isJust $ f s then f s else g s

-- eof завершается успехом только если на входе пустая строка.
-- parse eof "qwerty" == Nothing
-- parse eof "" == Just ((), "")
eof :: Parser ()
eof = Parser f where
    f "" = Just ((), "")
    f _  = Nothing 

