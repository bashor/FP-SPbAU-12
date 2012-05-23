{-# LANGUAGE TupleSections #-}

import Map

-- ??? после добавления maybe delete перестал компилится со следующим типом:
sort :: (Ord a, Num a) => [a] -> [a]
sort = map fst . toList . 
		(maybe empty id) . (delete (13)) .
		(maybe empty id) . (delete (56)) .
		(maybe empty id) . (delete (10)) .
		fromList . map (, ())

main = print $ sort [10,24,13,56,35,13,6,23] -- [6,10,13,23,24,35,56] -- [6,23,24,35]
