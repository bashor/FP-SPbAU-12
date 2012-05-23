#!/usr/bin/runghc

--b) cat. Если запускать без параметров, то копирует stdin в stdout. Если запускать с параметрами, выводит в stdout содержимое файлов, имена которых передаются в параметрах. Необходимо корректно обрабатывать ситуацию, когда какие-либо файлы не существуют.

import System.Environment
import Data.List
import System.IO

main = do
	args <- getArgs 
	if (args == [])
		then interact id
		else filesToOut args

readf f =
	do
		t <- readFile f
		return $ "=======\n" ++ f ++ "\n-------\n" ++ t ++ "\n\n"
	`catch`
		\e -> do 
			hPutStr stderr ("file " ++ f ++ " not found\n")
			return ""

filesToOut args = mapM readf args >>= mapM_ putStr