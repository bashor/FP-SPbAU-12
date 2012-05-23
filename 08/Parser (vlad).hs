module Parser
    ( Parser
    , empty
    , satisfy
    , failP
    , seqP
    , orP
    , eof
    , runParser
    , execParser
    ) where

import Control.Arrow

newtype Parser a = Parser { execParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap = undefined

-- runParser p s запускает парсер p на строке s.
runParser :: Parser a -> String -> Maybe a
runParser = undefined

-- empty x парсит пустую строку и возвращает x.
-- execParser (empty 4) "qwerty" == Just (4, "qwerty")
-- execParser (empty 'x') "" == Just ('x', "")
empty :: a -> Parser a
empty = undefined

-- failP всегда завершается неуспехом.
-- execParser failP "qwerty" == Nothing
-- execParser failP "" == Nothing
failP :: Parser a
failP = undefined

-- satisfy p парсит один символ и возвращает его, если он удовлетворяет предикату p. В противном случае неуспех.
-- execParser (satisfy (/= 'x')) "qwerty" == Just ('q', "werty")
-- execParser (satisfy (/= 'x')) "xwerty" == Nothing
satisfy :: (Char -> Bool) -> Parser Char
satisfy = undefined

infixl 3 `orP`
infixl 4 `seqP`

-- seqP p q парсит последовательно p и q, возвращает их результаты в паре.
-- execParser (satisfy (== 'q') `seqP` satisfy (== 'w')) "qwerty" == Just (('q', 'w'), "erty")
-- execParser (satisfy (== 'q') `seqP` satisfy (== 'x')) "qwerty" == Nothing
-- execParser (satisfy (== 'x') `seqP` satisfy (== 'w')) "qwerty" == Nothing
seqP :: Parser a -> Parser b -> Parser (a, b)
seqP = undefined

-- orP p q парсит p, если тот завершился неуспехом парсит q.
-- execParser (satisfy (== 'q') `orP` satisfy (== 'x')) "qwerty" == Just ('q', "werty")
-- execParser (satisfy (== 'x') `orP` satisfy (== 'q')) "qwerty" == Just ('q', "werty")
-- execParser (satisfy (== 'x') `orP` satisfy (== 'y')) "qwerty" == Nothing
orP :: Parser a -> Parser a -> Parser a
orP = undefined

-- eof завершается успехом только если на входе пустая строка.
-- execParser eof "qwerty" == Nothing
-- execParser eof "" == Just ((), "")
eof :: Parser ()
eof = undefined
