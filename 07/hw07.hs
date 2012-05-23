{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Char
import Data.List

--1. Написать instance Eq, Show, Read, Functor для типа Tree a b, который был реализован в предыдущем ДЗ.
--show и read должны работать следующим образом:
--show (Branch (Leaf 1) 2 (Branch (Leaf 3) 4 (Leaf 5))) == "<1{2}<3{4}5>>"
--(read "<1{2}<3{4}5>>" :: Tree Int Int) == Branch (Leaf 1) 2 (Branch (Leaf 3) 4 (Leaf 5))
data Tree a b = Leaf b | Branch (Tree a b) a (Tree a b) 

instance (Eq a, Eq b) => Eq (Tree a b) where
	(==) (Branch l1 v1 r1) (Branch l2 v2 r2) = v1 == v2 && l1 == l2 && r1 == r2
	(==) (Leaf v1) (Leaf v2) = v1 == v2
	(==) _ _ = False

leaf1 = (Leaf 1::Tree Int Int)
leaf2 = (Leaf 2::Tree Int Int)
node1 = (Branch leaf1 1 leaf1)
node2 = (Branch leaf1 1 leaf2)

--tests for Eq Tree
leafleaf_f = leaf1 == leaf2
leafleaf_t = leaf1 == leaf1
leafnode_f = leaf1 == node1
nodenode_t = node1 == node1
nodenode_f = node1 == node2

-----
instance (Show a, Show b) => Show (Tree a b) where
	--show (Leaf v) = show v
	--show (Branch l r v) = "<" ++ show l ++ "{" ++ show v ++ "}" ++ show r ++ ">"
	showsPrec _ (Leaf v) = shows v
	showsPrec _ (Branch l v r) = (showString "<") . shows l . 
		(showString "{") . shows v . (showString "}") . shows r . (showString ">")

instance (Read a, Read b) => Read (Tree a b) where
	readsPrec _ ('<':xs) = [(Branch l v r, u) | (l, '{' : vs) <- readsPrec 0 xs,
												(v, '}' : ls) <- readsPrec 0 vs,
												(r, '>' : u)  <- readsPrec 0 ls ]
	readsPrec _ xs = [(Leaf x, t) | (x,t) <- reads xs]

tree_test = (Branch (Leaf 1) 2 (Branch (Leaf 3) 4 (Leaf 5)))
tree_str = "<1{2}<3{4}5>>"
show_test = show tree_test == tree_str
read_test = (read tree_str :: Tree Int Int) == tree_test

instance Functor (Tree a) where
	--fmap :: (b -> d) -> (Tree a b) -> (Tree a d)
	fmap f (Branch l v r) = (Branch (fmap f l) v (fmap f r))
	fmap f (Leaf v) = (Leaf (f v))

--main = do
--	print tree_test
--	print show_test
--	print read_test
--	print $ fmap (*2) tree_test

--2. Для этого задания нужно подключить расширения языка. Для этого в начале файла необходимо написать следующую строчку: {-# LANGUAGE MultiParamTypeClasses #-}
--Объявим следующий класс типов:
class Conv a b where
  conv :: a -> b
--Смысл в том, что мы можем преобразовать значение типа a в значение типа b.
--Написать instance Conv для следующих пар типов:
--a) Int Double
instance Conv Int Double where
	conv = fromIntegral

--b) Double Int
instance Conv Double Int where
	conv = truncate 

--c) Int Char
instance Conv Int Char where
	conv = chr

--d) Char Int
instance Conv Char Int where
	conv = ord

--e) (Maybe a) [a]
instance Conv (Maybe a) [a] where
	conv (Just x) = [x]
	conv _ = []

--f) (Maybe a) Bool
instance Conv (Maybe a) Bool where
	conv (Just _) = True
	conv _ = False

--g) [a] [b]
instance (Conv a b) => Conv [a] [b] where
	conv = map conv

--h) (Maybe a) (Maybe b)
instance (Conv a b) => Conv (Maybe a) (Maybe b) where
	conv = fmap conv

--i) (a, b) (c, d)
instance (Conv a c, Conv b d) => Conv (a, b) (c, d) where
	conv (x, y) = (conv x, conv y)

--j) (a -> b) (c -> d)
instance (Conv c a, Conv b d) => Conv (a -> b) (c -> d) where
	conv f = conv . f . conv

--3. В этом задании необходимо распарсить лог-файл в некотором формате. Пример есть в файле error.log.

--Формат следующий: в начале идет одна буква: E, W или I, что означает error, warning или info соответственно. У ошибок (и только у них) есть уровень - это число от 1 до 99. Он идет сразу за буквой E. После буквы W или I или E с уровнем идет timestamp. Потом идет сообщение. Также некоторые строки не находятся в данном формате, их нужно корректно обрабатывать.

--a) Реализовать тип данных LogMessage для представления строки лога (в том числе и строк, которые не находятся в правильном формате).

data LogMessage = Warning Integer String | Info Integer String | Error Integer Integer String | Other String

--b) Написать instance Read и Show для LogMessage. show должен выдавать строку в том же формате.

showLine prefix timestamp message = prefix . showString " ". shows timestamp . showString " ". showString message

instance Show LogMessage where
	showsPrec _ (Error level timestamp message) = showLine (showString "E " . shows level) timestamp message
	showsPrec _ (Info timestamp message) =  showLine (showString "I") timestamp  message
	showsPrec _ (Warning timestamp message) = showLine (showString "W") timestamp message
	showsPrec _ (Other message) = showString message

instance Read LogMessage where
	readsPrec _  l@(t : ' ': xs) = 
		if t == 'E' then
			case (reads xs::[(Integer, String)]) of
				(level, ' ' : s) : _ | level >= 1 && level <= 99 ->
					case (reads s::[(Integer, String)]) of
						(timestamp, ' ' : message) : _ -> [(Error level timestamp message, "")]
						_ -> [(Other l, "")]
				_ -> [(Other l, "")]

		else
			case (reads xs::[(Integer, String)]) of
				(timestamp, ' ' : message) : _ ->
					case t of
						'W' -> [(Warning timestamp message, "")]
						'I' -> [(Info timestamp message, "")]
				_ -> [(Other l, "")]

	readsPrec _  xs = [(Other xs, "")]

--main = do
--	print $ lex "W a s t"
--	print $ (reads "123"::[(Int, String)])
--	print (read "#66WW-1a.2:e680fed13 00:1a00: adevicesetlindow [0xa0] (idged" :: LogMessage)
--	print (read "W a" :: LogMessage)
--	print (read "W 1a" :: LogMessage)
--	print (read "I a" :: LogMessage)
--	print (read "E a" :: LogMessage)
--	print (read "E 1 a" :: LogMessage)
--	print (read "W 1654 'I kept all my limbs very supple" :: LogMessage)
--	print (read "I 3974 #55500:00000 (nux Us nel chablesen ster C)" :: LogMessage)
--	print (read "E 47 1034 'What a pity it wouldn't stay!' sighed the Lory, as soon as it was quite" :: LogMessage)

--c) Написать instance Ord для LogMessage. Он должен работать следующим образом: Наибольшими считаются ошибки, причем между собой они сравниваются по своему уровню. Затем идут warning, потом info, потом строки в неправильном формате. Между собой сообщения одного типа (например info и info) сравниваются по timestamp.
instance Eq LogMessage where
	(Error l1 t1 m1) == (Error l2 t2 m2) = l1 == l2 && t1 == t2
	(Warning t1 m1) == (Warning t2 m2)	 = t1 == t2
	(Info t1 m1) == (Info t2 m2)		 = t1 == t2
	_ == _								 = False

instance Ord LogMessage where
	(Error l1 t1 _) <= (Error l2 t2 _) = l1 < l2 || (l1 == l2 && t1 <= t2)
	(Error _ _ _) <= _ = False
	(Warning t1 _) <= (Warning t2 _) = t1 <= t2
	(Warning _ _) <= (Error _ _ _) = True
	(Warning _ _) <= _ = False
	(Info t1 _) <= (Info t2 _) 		 = t1 <= t2
	(Info _ _) <= (Error _ _ _) = True
	(Info _ _) <= (Warning _ _) = True
	(Info _ _) <= _ = False
	(Other _) <= (Other _) = False
	(Other _) <= _ = True

--main = do
--	print $ (Error 1 3 "") >= (Warning 3 "sss")
--	print $ (Info 1744 "aloud; and in another moment it was out of sight.") <=
--			(Error 47 1034 "'What a pity it wouldn't stay!' sighed the Lory, as soon as it was quite")


--d) Написать функцию processLog :: String -> String, которая парсит лог, потом сортирует так, чтобы ошибки шли в первую очередь, потом опять превращает в строчку.
--Не забывайте про функции lines и unlines, они вам пригодятся. Для тестирования можно использовать следующий код: 

processLog = unlines . map show . sortBy (flip compare) . map (read::(String->LogMessage)) . lines

main = readFile "error.log" >>= writeFile "sorted.log" . processLog

--4. В этом задании необходимо написать интерпретатор для простого языка с си подобным синтаксисом.
--Я описал все необходимые типы данных, они находятся в файле Expr.hs. Необходимо разобраться с этими определениями.
--Также я написал парсер для него, он находится в файле Parsing.hs. Разбираться в нем не обязательно, но можно. Только учтите, что я там использовал несколько вещей, которые мы еще не проходили, а некоторые и не пройдем.
--В файле Main.hs лежит несколько примеров. Также там находятся функции evalExpr, evalStat и evalProg, которые вам необходимо реализовать.
--type Store - это мап, который хранит текущее значение переменных.
--Функция evalExpr возвращает либо вычисленное значение выражения, либо список ошибок. Ошибки бывают двух видов: неизвестная переменная и несоответствие типов.
--Функции evalStat и evalProg возвращают список ошибок (если ошибок не было, список пустой) и обновленный Store.

--Для работы моего кода вам понадобится библиотека pretty. Устанавливается она так: cabal install pretty.