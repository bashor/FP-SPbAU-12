#!/usr/bin/runghc

--5. Напишите telnet. Он должен коннектиться к указонному в параметрах серверу по указанному порту. Весь ввод в stdin он отправляет на сервер, а все ответы сервера выводит в stdout.

import System.IO
import System.Environment
import Network
import Control.Concurrent

main = do
	args <- getArgs
	let count = length args
	case args of
		(host : port : _) -> withSocketsDo $ do
			h <- connectTo host $ PortNumber $ fromIntegral (read port :: Int)
			--hSetBuffering h NoBuffering
			forkIO $ reader h
			writer h
		_ -> putStr "Usage: telnet <hostname> <port>"

reader h = do
	t <- hGetLine h
	putStrLn t
	hFlush stdout
	reader h

writer h = do
	t <- getLine
	case t of ~"exit" -> do
			hPutStrLn h t
			hFlush h
			writer h