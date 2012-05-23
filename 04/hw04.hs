main = print $ integral sin 0 pi 

--1.
--Реализовать b через s и k.
b x y z = x (y z)
k x y = x
s x y z = x z (y z)

b' f = s (k f)

--2.
plus = (+)
--Написать mult (умножение) через plus.
mult a b | a == 0 || b == 0 = 0
mult a 1 = a
mult a b = a `plus` mult a (b - 1)

--Написать expt (возведение в степень) через mult.
-- a^b
expt a 0 = 1
expt 0 b = 0
expt a b = a `mult` expt a (b - 1)

--3. Реализовать следующую функцию http://en.wikipedia.org/wiki/McCarthy_91_function
m n | n > 100 = n - 10
m n | n <= 100 = m $ m $ n + 11

--4. Реализовать оператор минимизации minp :: (Integer -> Bool) -> Integer

-- ?? чем это отличается от того что мы делали на паре?
minp f = minp' f 0
minp' f i | (f i) = i
          | otherwise = minp' f (i + 1)

--5.
--a) Написать функцию, возвращающую количество цифр числа.
ccount 0 = 1
ccount n = ccount' 0 n
			where
				ccount' s 0 = s
				ccount' s n = ccount' (s + 1) (n `div` 10)
--b) Написать функцию, возвращающую сумму цифр числа.
csum n = csum' 0 n
			where
				csum' s 0 = s
				csum' s n = csum' (s + (n `mod` 10)) (n `div` 10)

--6. Реализовать функцию gcd при помощи алгоритма Евклида.
gcd' x 0 = x
gcd' x y = gcd' y (x `mod` y)

--7. Реализовать функцию, находящую значение определенного интеграла на отрезке методом трапеций.
integral f minv maxv = integral' minv maxv 0.1 0
	where
		integral' minv maxv h s | minv + h >= maxv = s + (calcSegment $ maxv - minv)
								| otherwise        = integral' (minv + h) maxv h (s + (calcSegment h))
			where
				calcSegment h = ((f minv) + (f $ minv + h)) * h / 2

