{-# LANGUAGE FlexibleInstances #-}

import Fail
import FailMsg

-- Возвращает квадратный корень. Завершается неуспехом, если число < 0.
sqrtF :: Double -> Fail Double
sqrtF a | a < 0 = abort
		| otherwise = return $ sqrt a


--Тоже самое. В случае ошибки выдает какое-нибудь сообщение.
sqrtE :: Double -> FailMsg String Double
sqrtE a | a < 0 = abortMsg "beda beda a<0"
		| otherwise = return $ sqrt a

-- sqrtSumF x y возвращает сумму корней x и y (т.е. sqrt x + sqrt y)
-- Используйте do-нотацию.
sqrtSumF :: Double -> Double -> Fail Double
sqrtSumF x y = do
	xx <- sqrtF x
	yy <- sqrtF y
	return $ xx + yy

-- Тоже самое.
sqrtSumE :: Double -> Double -> FailMsg String Double
sqrtSumE x y = do
	xx <- sqrtE x
	yy <- sqrtE y
	return $ xx + yy

-------------------------------------------------------------------------

-- После того как вы реализуете sqrtSumF и sqrtSumE можно заметить,
-- что код у них почти одинаковый (по крайней мере, так должно у вас получится).
-- Чтобы избежать дублирования кода обычно поступают так:
-- создают класс типов, который расширяет Monad, добавляя необходимые функции.
-- В нашем случае это функция, вычисляющая квадратный корень.

class Monad m => MonadSqrt m where
    sqrtM :: Double -> m Double

-- Теперь можно написать обобщенную версию sqrtSum (которая будет выглядеть точно также, как и предыдущие).
sqrtSum :: MonadSqrt m => Double -> Double -> m Double
sqrtSum x y = do
	xx <- sqrtM x
	yy <- sqrtM y
	return $ xx + yy

-- И реализовать instance (что несложно, т.к. все необходимое уже есть).
instance MonadSqrt Fail where
    sqrtM = sqrtF

instance MonadSqrt (FailMsg String) where
    sqrtM = sqrtE

-- Тесты.
main = do
    let tests = [(1,2), (-1,2), (1,-2), (-1,-2)]
    mapM_ (print . runFail . uncurry sqrtSum) tests
    mapM_ (print . runFailMsg' . uncurry sqrtSum) tests
  where
    runFailMsg' :: FailMsg String Double -> Either String Double
    runFailMsg' = runFailMsg

-- Вывод:
-- Just 2.414213562373095
-- Nothing
-- Nothing
-- Nothing
-- Right 2.414213562373095
-- Left <что-то там>
-- Left <что-то там>
-- Left <что-то там>
