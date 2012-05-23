module Fp09 where
import Control.Monad.Instances
import Control.Applicative
import Control.Monad.Identity

-- Из произвольной функции с помощью return 
-- можно сделать стрелку Клейсли 
toKleisli :: Monad m => (a -> b) -> (a -> m b)
toKleisli f = \x -> return (f x) 

-- Разворачивает конвеер вычислений
euro :: a -> (a -> b) -> b
euro = flip ($)

----------------------------------------
-- монада Identity 
wrap'n'succ :: Integer -> Identity Integer 
wrap'n'succ x = Identity (succ x)

-- runIdentity $ wrap'n'succ 3 >>= wrap'n'succ >>= wrap'n'succ
-- вернёт 6

goWrap0 = wrap'n'succ 3 >>= 
          wrap'n'succ >>= 
          wrap'n'succ >>= 
          return

-- применим третий закон монад несколько раз
goWrap1 = wrap'n'succ 3 >>= (\x -> 
          wrap'n'succ x >>= (\y -> 
          wrap'n'succ y >>=  \z -> 
          return z))

-- можем обращаться к распакованным результатам промежуточных вычислений
goWrap2 = wrap'n'succ 3 >>= (\x -> 
          wrap'n'succ x >>= (\y -> 
          wrap'n'succ y >>=  \z -> 
          return (x,y,z)))

goWrap2' = wrap'n'succ 3 >>= (\x -> 
           wrap'n'succ x >>= (\x -> 
           wrap'n'succ x >>=  \x -> 
           return x))


-- let-связывание позволяет использовать обычные переменные
goWrap3 = let i = 3 in
          wrap'n'succ i >>= (\x -> 
          wrap'n'succ x >>= (\y -> 
          wrap'n'succ y >>=  \z -> 
          return (i,x,y,z)))

-- если результат какого-то вычисления не интересен, переменную можно не вводить 
goWrap4 = let i = 3 in
          wrap'n'succ i >>= (\x -> 
          wrap'n'succ x >>= (\y -> 
          wrap'n'succ y >> 
          return (i,x,y)))

-- то же самое удобно записывать в do-нотации
goWrap5 = do 
          let i = 3
          x <- wrap'n'succ i
          y <- wrap'n'succ x
          wrap'n'succ y
          return (i,x,y)

goWrap5' = do 
          let i = 3
          x <- wrap'n'succ i
          x <- wrap'n'succ x
          x <- wrap'n'succ x
          return (i,x)

----------------------------------------
-- монада Maybe
type Name = String
type DataBase = [(Name, Name)]

fathers, mothers :: DataBase
fathers = [("Bill","John"),("Ann", "John"), ("John", "Piter")]
mothers = [("Bill","Jane"),("Ann", "Jane"), ("John", "Alice"),("Jane", "Dorothy"), ("Alice", "Mary")]

-- стрелки Клейсли для Maybe
getM, getF :: Name -> Maybe Name
getM = \p -> lookup p mothers
getF = \p -> lookup p fathers

-- ищем прабабушку по материнской линии отца
-- *Fp08> getF "Bill" >>= getM >>= getM
-- Just "Mary"
-- *Fp08> do {f <- getF "Bill"; m <- getM f; getM m}
-- Just "Mary"

-- Ищем бабушек
granmas person = do
  m   <- getM person
  gmm <- getM m
  f   <- getF person
  gmf <- getM f
  return (gmm, gmf)

-- *Fp08> granmas "Ann"
-- Just ("Dorothy","Alice")
-- *Fp08> granmas "John"
-- Nothing

granmas' person = do
  m   <- getM person
  gmm <- getM m
  f   <- getF person
--  gmf <- getM f
  return gmm



----------------------------------------
-- монада списка

-- Следующие три списка --- это одно и то же

list1 = [(x,y) | x <- [1,2,3] , y <- [1,2,3], x /= y]

list2 = do 
   x <- [1,2,3] 
   y <- [1,2,3] 
   True <- return (x /= y) 
   return (x,y)

list3 = 
  [1,2,3] >>= (\x -> 
  [1,2,3] >>= (\y -> 
  return (x/=y) >>= (\r -> 
  case r of True -> return (x,y)
            _    -> fail "Will be ignored :)")))




