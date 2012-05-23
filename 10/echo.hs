#!/usr/bin/runghc

--a) echo. Должен выводить на консоль свои аргументы. Реализуйте упрощенную версию, никаких ключей обрабатывать не надо.

import System.Environment
import Data.List

main = getArgs >>= (putStrLn . (intercalate " "))