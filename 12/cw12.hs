--monad transformers
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

type MyType = WriterT String (ReaderT Int (StateT Bool IO)) ()

f :: State String Int
f = do
	s <- get 
	put ('x':s)
	return $ length s

main = print $ runState f

--type State = StateT Identity
--type Counter = CounterT Identity
newtype MaybeT m a = MaybeT {runMabyT :: m (Maybe a)}

--class 

instance Monad m => Monad (MaybeT m) where
	return a = MaybeT $ return $ Just a

	--(>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
	MaybeT a >>= f = do
		y <- a
		case y of
			Just x -> f x
			Nothing -> MaybeT $ return $ Nothing