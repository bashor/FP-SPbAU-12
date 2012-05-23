{-# LANGUAGE ParallelListComp #-}
import Control.DeepSeq

--1. Дан список. Написать функцию, возвращающую список пар: элемент и его номер, т.е. f "abc" == [('a',0),('b',1),('c',2)]
--a) При помощи рекурсии.
--main = print $ f "abc" -- == [('a',0),('b',1),('c',2)]
f :: (Integral b) => [a] -> [(a, b)]
f x = f' x 0
		where
			f' [] _ = []
			f' (x : xs) i = (x, i) : (f' xs (i + 1))
--b) При помощи функции zip.
--main = print $ ff "abc" -- == [('a',0),('b',1),('c',2)]
f2 :: (Integral b) => [a] -> [(a, b)]
f2 = flip zip [0..]
 
--2. Написать функцию fun :: [Int] -> [Int], которая четные числа в нечетных позициях (нумеруя с 0) умножает на 2, остальные не изменяет.
--    fun [1,3,6,10,15,21,28,30,60] == [1,3,6,20,15,21,28,60,60]
--main = print $ fun' [1,3,6,10,15,21,28,30,60] -- == [1,3,6,20,15,21,28,60,60]
--main = print $ fun' [2,2,2,2]

fun :: (Integral a) => [a] -> [a]
fun x = fun' x [0..]
			where
				fun' :: (Integral a) => [a] -> [a] -> [a]
				fun' [] _ = []
				fun' (x : xs) (y : ys) | even x && odd y = (x * 2) : (fun' xs ys)
									   | otherwise = x : (fun' xs ys)

fun' [] = []
fun' [x] = [x]
fun' (x:y:xs) | even y = x : 2 * y : fun xs | otherwise = x : y : fun xs

--3. Реализовать reverse.
--a) При помощи (++)
--main = print $ reverseA [1..9]
reverseA :: [a] -> [a]
reverseA xs = reverseA' xs []
				where
					reverseA' [] s = s
					reverseA' (x : xs) s = reverseA' xs ([x] ++ s)

reverseA2 :: [a] -> [a]
reverseA2 [] = []
reverseA2 (x:xs) = (reverseA2 xs) ++ [x]

main = reverseA ([1..10000] :: [Int]) `deepseq` return ()
-- ??? какой вариант лучше, когда и почему?
-- не знаю как лучше тестить, резульататы тестов при запуске в консоле time runhaskell hw05.hs сильно противоречат
-- взуальным ощущениям при запуске внутри sublime
-- как лучше тестирать?
-- 2й вариант вроде отличается только прожерливостью стека.

--b) При помощи аккумулирующего параметра.
reverseB :: [a] -> [a]
reverseB xs = reverseB' xs []
				where
					reverseB' [] ys = ys
					reverseB' (x:xs) ys = reverseB' xs (x:ys)
--Сравнить их время работы на списке [1..10000]

--4. Реализовать следующие функции, используя композицию:
--a) nelem :: a -> [a] -> Bool, которая работает как функция notElem. Используйте функцию elem.
nelem :: (Eq a) => a -> [a] -> Bool
nelem x = not . elem x
--b) f :: (Int -> Int) -> Int -> Bool. f g x должен возвращать True, если g x четен. Используйте функцию even.
fb :: (Int -> Int) -> Int -> Bool
fb g = even . g
--c) f :: [Int] -> Bool. f xs возвращает True, если в xs есть хотя бы 1 положительное число, иначе False. Используйте функции filter и null.
fc :: [Int] -> Bool
fc = not . null . filter (>0)

fc2 :: [Int] -> Bool
fc2 = any (>0)
--main = print $ fc [-1, 2, -1, -2, 3]
--d) f :: (a -> Bool) -> [a] -> Int. f p xs возвращает количество элементов в xs, не удовлетворяющих предикату p. Используйте функции filter и length.
fd :: (a -> Bool) -> [a] -> Int
fd p = length . filter p

fd2 :: (a -> Bool) -> [a] -> Int
fd2 p xs = fd2' p xs 0
	where 
		fd2' _ [] c = c
		fd2' p (x:xs) c | (p x) = fd2' p xs (c + 1)
					    | otherwise = fd2' p xs c

--main = print $ fd2 (even) [1..20] -- == 10
--e) f :: [Int] -> Int. f возвращает сумму первых 10 элементов списка.
fe :: [Int] -> Int
fe = sum . take 10
--main = print $ fe [0..20] -- == 45
--f) f :: [Int] -> Int. f каждый элемент умножает на 2, потом прибавляет 3 и возвращает произведение всех элементов. f [1,2,3] == 315. Используйте функцию product.
ff :: [Int] -> Int
ff = product . map ((+3) . (*2))
--main = print $ ff [1,2,3] -- == 315

--5. primes :: [Integer] -- бесконечный список простых чисел. Hint: напишите сначала функцию isPrime, проверяющую простоту числа.
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime p | p < 1 = False
		  | even p = False
isPrime p = isPrime' p (truncate $ sqrt $ fromInteger p)
			where
				isPrime' :: Integer -> Integer -> Bool
				isPrime' p 1 = True
				isPrime' p c | p `mod` c == 0 = False
							 | otherwise = isPrime' p (c - 1)

primes :: [Integer]
primes = 2 : [x | x <- [3..], isPrime' x]

--main = print $ take 10 primes

-- ??? как работает foldr для бесконечных списков? почему и как работает вот это:
isPrime' x = x > 1 && foldl (\r p-> p*p>x || (x `mod` p /= 0 && r)) True primes

--isPrime' x = x > 1 && isPrime'' primes
--  where
--    isPrime'' (p:ps) = p * p > x || (x `mod` p /= 0 && isPrime'' ps)

--main = print $ isPrime'' (10)
--main = print $ take 10 primes

--6. fibs :: [Integer] -- бесконечный список чисел фибоначчи.
fibs = 0 : 1 : [a + b | a <- fibs | b <- tail fibs]
--main = print $ take 10 fibs

--7. swap :: Int -> Int -> [a] -> [a] -- swap i j меняет местами i и j элементы. Например swap 1 2 [3,4,5,6] == [3,5,4,6]. swap 2 0 "abcd" == "cbad".
--main = print $
--	--swap 1 2 [3,4,5,6] -- == [3,5,4,6]
--	swap 2 0 "abcd" -- == "cbad"
swap i j s | i == j = s
           | i > j = swap j i s
           | otherwise = a ++ (y : xs) ++ (x : ys)
							where
								(a, ss) = splitAt i s
								((x : xs), (y : ys)) = splitAt (j-i) ss

--8. Задана рекурентная последовательность a(0) = 1, a(1) = 2, a(2) = 3, a(k+3) = a(k+2) + a(k) - 2*a(k+1).
--    Написать функцию f :: Int -> Int. f i возвращает i элемент этой последовательности. f Должна работать линейное время. Нельзя использовать списки. Hint: используйте кортежи.

f8 0 = 1
f8 1 = 2
f8 2 = 3
f8 i = f8' 3 i 1 2 3
f8' i n a0 a1 a2 | i > n  = a2
				 | i <= n = f8' (i + 1) n a1 a2 (a2 + a0 - 2 * a1)

--[0,1,2,3, 4, 5,6,7,  8,  9]
--[1,2,3,0,-4,-1,7,5,-10,-13]
--main = print $ f8 9