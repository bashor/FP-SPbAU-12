import qualified Data.Map as M

-- К функциям из Data.Map обращайтесь с префиксом M.
-- Т.е. вместо lookup используйте M.lookup

-- f достает из мапы значения по ключам "x", "y" и "z".
-- Если хотя бы одного из них нет, возвращает Nothing.
-- Иначе возвращает сумму значений.
-- Используйте do-нотацию.
f :: M.Map String Int -> Maybe Int
f m = do
	x <- M.lookup "x" m
	y <- M.lookup "y" m
	z <- M.lookup "z" m
	return $ x + y + z

lookup' :: (Show a, Ord a) => a -> M.Map a b -> Either String b
lookup' a m = maybe (Left $ show a ++ " not found") Right (M.lookup a m)

-- Тоже самое, что и f, только вместо M.lookup используйте lookup'
g :: M.Map String Int -> Either String Int
g m = do
	x <- lookup' "x" m
	y <- lookup' "y" m
	z <- lookup' "z" m
	return $ x + y + z

-- Тесты
tests = map M.fromList
    [ [("a",1),("y",2),("z",3)]
    , [("x",1),("y",2),("a",3)]
    , [("x",1),("y",2),("z",3)]
    ]

main = do
    mapM_ (print . f) tests
    mapM_ (print' . g) tests
    where print' (Left s) = putStrLn s
          print' (Right a) = print a

-- Вывод:
-- Nothing
-- Nothing
-- Just 6
-- "x" not found
-- "z" not found
-- 6
