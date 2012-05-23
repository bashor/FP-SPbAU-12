import Data.Char
import Control.Applicative

newtype Parser a = Parser {runParser'::String -> Maybe (a, String)}
--orP  :: Parser a -> Parser a -> Parser a
--seqP :: Parser a -> Parser b -> Parser (a,b)
--plusP, starP  :: Parser a -> Parser [a]
--runParser :: Parser a -> String -> Maybe a


instance Functor Parser where
	-- fmap :: (a -> b) -> Parser a -> Parser b
	fmap f (Parser g)= Parser h
	-- f :: a -> b
	-- g :: String -> Maybe (a, String)
		where
			--h:: String -> Maybe (b, String)
			h s = fmap (\(a, s') -> (f a, s')) (g s)

--empty:: a -> Parser a
--empty x = Parser (\s -> Just (x, s))

--satisfy::(Char -> Bool) -> Parser Char
--satisfy f = Parser (\s ->if f (head s) then Just (head s, tail s) else empty (head s))

--digit = satisfy isDigit
--nat = plusP digit
--int = nat `orP` (satisfy (=='-') `seqP` nat)
--orP x y = maybe (maybe  (runParser y)) (runParser x)
--starP p = fmap (\(a,b) -> a : b) (p `seqP` starP p) `orP` empty [] 

instance Applicative Parser where
	-- a -> Parser a
	pure x = Parser (\s -> Just (x, s))
	-- Parser (a -> b) -> Parser a -> Parser b
	-- f :: String -> Maybe (a->b, String)
	-- g :: String -> Maybe (a, String)
	(<*>) (Parser f) (Parser g) = Parser h
		where
			h s = case f s of
				Just (x, xs) -> case g xs of
					Just (y, ys) -> Just (x y, ys) 
					_ 			 -> Nothing
				_ 			 	 -> Nothing

instance Alternative Parser where
	empty = Parser h
		where
			h _ = Nothing
	(<|>) (Parser f) (Parser g) = Parser h
		where
    		h s = case f s of
    			m@(Just (x, xs)) -> m
    			_ 			   -> f s

