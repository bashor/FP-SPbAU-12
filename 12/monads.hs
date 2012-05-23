import Control.Monad.State
import Control.Monad.Reader

forState :: (MonadState s m, Num s) => s -> s -> m () -> m ()
forState b e m
	| b /=e = do 
		--put b
		execState m b
		forState (b + 1) e m
	| otherwise = return ()

forReader :: (MonadReader s m, Num s) => s -> s -> m () -> m ()
forReader = undefined

main = do
    flip runStateT (error "foo") $ forState 0 5 $ do
        i <- get
        liftIO $ putStr (show i ++ " ")
    flip runReaderT (error "bar") $ forReader 5 10 $ do
        i <- ask
        liftIO $ putStr (show i ++ " ")
    putStrLn ""
