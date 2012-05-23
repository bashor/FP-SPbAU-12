#!/usr/bin/runghc
{-# LANGUAGE ScopedTypeVariables #-}

--4. Напишите эхо-сервер, который ждет на каком-либо порту клиентов и каждую строчку, присланную клиентом, отправляет обратно.
import System.IO
import Network
import Control.Concurrent

port = PortNumber 12345

main = withSocketsDo $ do
	sock <- listenOn port 
	handler sock

handler s = do
	(h, host, _) <- accept s 
	--hSetBuffering h NoBuffering
	--forkIO (hGetContents h>>= hPutStrLn h)
	forkIO $ echo h (host ++ ":")
	handler s

echo h host = do
	text <- hGetLine h
	putStr host
	print text
	hPutStrLn h text
	hFlush h
	echo h host