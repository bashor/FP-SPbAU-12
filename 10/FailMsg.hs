module FailMsg
    ( FailMsg
    , abortMsg
    , runFailMsg
    ) where

-- Вычисления, которые могут завершатся неуспехом с сообщением об ошибке.
data FailMsg e a = Fail e | Ok a

instance Monad (FailMsg e) where
    Ok a >>= f   = f a
    Fail e >>= _ = Fail e
    return = Ok

-- abortMsg e остановливает вычисления с сообщением e.
abortMsg :: e -> FailMsg e a
abortMsg = Fail

-- Запускает вычисления. Возвращает Left <сообщение об ошибке>, либо Right <результат>.
runFailMsg :: FailMsg e a -> Either e a
runFailMsg (Fail e) = Left e
runFailMsg (Ok a)   = Right a
