module Eval
    ( Eval
    , runEval
    , getVar
    , update
    ) where

import qualified Data.Map as M
import Control.Applicative hiding (Const)
import Control.Monad

import Expr

newtype Eval a = Eval { runEval :: M.Map String Value -> (Maybe a, [String], M.Map String Value) }

instance Functor Eval where
    fmap f (Eval e) = f 

instance Applicative Eval where
    pure = undefined
    (<*>) = undefined

instance Monad Eval where
    return = undefined
    (>>=) = undefined
    --fail = Nothing

-- MonadPlus - тоже самое, что и Alternative, только для монад
instance MonadPlus Eval where
    mzero = undefined
    mplus = undefined

-- update "x" v устанавливает значение переменной "x" в v.
update :: String -> Value -> Eval ()
update = undefined

-- getVar "x" возвращает текущее значение переменной "x"
getVar :: String -> Eval Value
getVar = undefined
