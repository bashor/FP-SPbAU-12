--import Prelude(print, (+), )
import Control.Monad.Fix

main = print $ min' (==6)

plus a b = a + b
plus' = (+)
plus'' a b = \a b -> a + b

mult a b = mult' a b
           where
             mult' a b | a == 0 || b == 0 = 0
             mult' a 1 = a
             mult' a b = a + mult a (b - 1)

i m = m -- id
k m n = m -- const
s m n l = m l (n l) -- 
b m n l = m (n l) -- (.)

--b' :: (t1 -> t) -> (t2 -> t1) -> t2 -> t
--b' m n l = f ((flip' k) s)) m n l

flip' :: (a->b->c)->(b->a->c)
flip' f = \a b -> f b a

sign x | x == 0 = 0
       | x > 0 = 1
       | x < 0 = -1

min' f = min'' f 0
--min''= fix $ \ff f i -> if (f i) then i else ff f (i + 1)
min'' f i | (f i) = i
          | otherwise = min'' f (i + 1)
