module Fail
    ( Fail
    , abort
    , runFail
    ) where

-- Вычисления, которые могут завершатся неуспехом.
data Fail a = Ok a | Fail

instance Monad Fail where
    Ok x >>= f = f x
    Fail >>= _ = Fail
    return = Ok

-- Остановить вычисления.
abort :: Fail a
abort = Fail

-- Возвращает Nothing, если вычисления завершились неуспехом, иначе Just <результат>.
runFail :: Fail a -> Maybe a
runFail Fail   = Nothing
runFail (Ok a) = Just a
