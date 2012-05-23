import Debug.Trace

--1. Напишите функцию for :: Monad m => Integer -> Integer -> (Integer -> m ()) -> m ()
--так, чтобы for 0 5 (\i -> print i) выводил бы числа 0, 1, 2, 3, 4.
for :: Monad m => Integer -> Integer -> (Integer -> m ()) -> m ()
for b e m = mapM_ m [b..e-1]

--main = for 0 5 (\i -> print i) 

--3.
--a) Напишите монаду Counter. Она должна поддерживать операции
--tick :: Counter ()
--runCounter :: Counter a -> (a, Int) -- возвращает результат и количество тиков.

data Counter a = Counter a Int

instance Monad Counter where
	return a = Counter a 0
	Counter a c >>= f = let (Counter aa cc) = f a in Counter aa (cc + c)

tick :: Counter ()
tick = Counter () 1

runCounter :: Counter a -> (a, Int)
runCounter (Counter a v) =  (a, v)

--b) Напишите функции
filter' :: (a -> Bool) -> [a] -> Counter [a]
filter' f [] = tick >> return []
filter' f (x:xs) = do
	tick
	if f x then do 
		r <- filter' f xs
		return (x : r)
	else 
		filter' f xs

--main = print $ runCounter $ filter' even [1..10]

append :: [a] -> [a] -> Counter [a]
append [] a = tick >> return a
append (x:xs) y = do 
	tick
	a <- append xs y
	return (x : a)

--main = print $ runCounter $ append [1..5] [1..5]

qsort :: Ord a => [a] -> Counter [a]
qsort []     = tick >> return []
qsort (p:xs) = do
	tick
	lesser <- filter' (<p) xs
	a <- qsort lesser
	greater <- filter' (>=p) xs
	b <- qsort greater
	append a (p:b)

--являющиеся аналогами функций filter, (++), sort, но считающие количество шагов.
--Убедитесь, что список [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15] сортируется быстрее, чем [1..15]

main = do
	print $ runCounter $ qsort [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
	print $ runCounter $ qsort [1..15]
