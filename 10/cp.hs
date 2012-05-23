#!/usr/bin/runghc

--c) cp. Копирует один файл в другой. Реализуйте упрощенную версию. Нужно уметь копировать только 1 файл. Как и с cat, не забывайте про то, что файл может не существовать.

import System.Environment
import Data.List
import System.IO

main = do
	args <- getArgs 
	let argc = length args
	if (argc < 2)
		then putStrLn "Usage: cp <src> <dst>"
		else cp (args !! 0) (args !! 1)

cp src dst =
	(readFile src >>= writeFile dst)
	`catch`
		\e -> do
			hPutStr stderr (show e)