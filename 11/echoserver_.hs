{-# LANGUAGE ScopedTypeVariables #-}
--4. Напишите эхо-сервер, который ждет на каком-либо порту клиентов и каждую строчку, присланную клиентом, отправляет обратно.
import System.IO
import Network
import Control.Concurrent
import Control.Monad
import System.Posix

port = PortNumber 6000

main = withSocketsDo $ do
	sock <- listenOn port 
	handler sock

handler :: Socket -> IO ()
handler s = do
	--installHandler sigPIPE Ignore Nothing
	(h, host, _) <- accept s 
	hSetBuffering h NoBuffering
	forkIO $ echo h (host ++ ":")
	handler s

echo h host = do
	(r, text) <-
		liftM (\t -> (True, t)) (hGetLine h) 
		`catch`
		\e -> return (False, "")
	if r then do
		putStr host
		print text
		hPutStrLn h text
		echo h host
	else do
		putStr host ++ "disconected"
		hClose h
			