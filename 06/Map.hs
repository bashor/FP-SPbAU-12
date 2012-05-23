module Map
    ( Map
    , empty	
    , lookup
    , insert
    , delete
    , fromList
    , toList
    ) where

import Prelude hiding (lookup)

data Map k v = Node k v (Map k v) (Map k v) | Null deriving Show

empty :: Map k v
empty = Null

lookup :: Ord k => k -> Map k v -> Maybe v
lookup _ Null = Nothing
lookup kk (Node nk nv l r) = case compare kk nk of
    EQ -> Just nv
    LT -> lookup kk l
    GT -> lookup kk r

-- Возвращает обновленное дерево и Nothing, если до этого в дереве не было ключа k, и Just v, если в дереве по ключу k было значение v
insert :: Ord k => k -> v -> Map k v -> (Map k v, Maybe v)
insert kk vv Null = (Node kk vv Null Null, Nothing)
insert kk vv (Node nk nv l r) | kk == nk = (Node kk vv l r, Just nv)
insert kk vv (Node nk nv l r) | kk < nk   = let (map_, maybe_) = (insert kk vv l) in (Node nk nv map_ r, maybe_)
insert kk vv (Node nk nv l r) | kk > nk   = let (map_, maybe_) = (insert kk vv r) in (Node nk nv l map_, maybe_)

-- Возвращает Nothing, если ключа k нет в дереве и Just t, если есть, где t - дерево с удаленным ключом k
delete :: Ord k => k -> Map k v -> Maybe (Map k v)
delete _ Null = Nothing
delete kk (Node nk nv Null r)    | kk == nk = Just r
delete kk (Node nk nv l Null)    | kk == nk = Just l
delete kk (Node nk nv l r)       | kk == nk = Just (merge l)
	where
		merge Null = r
		merge (Node k2 v2 l2 r2) = Node k2 v2 l2 (merge r2)
delete kk (Node nk nv l r)       | kk < nk  = maybe Nothing (\m -> Just (Node nk nv m r)) (delete kk l)
delete kk (Node nk nv l r)       | kk > nk  = fmap (Node nk nv l) (delete kk r)

fromList :: Ord k => [(k, v)] -> Map k v
fromList = fromList' Null
	where
		fromList' m [] = m
		fromList' m ((kk, vv) : l) = fromList' (fst (insert kk vv m)) l

-- Обход в инфиксном порядке
toList :: Map k v -> [(k, v)]
toList Null = []
toList (Node kk vv l r) = ((toList l) ++ ((kk, vv) : (toList r)))

null_m = Null::(Map Int Int)
map_m = delete (13::Int) $ fst $ insert 1 1 Null
map_2 = delete (2::Int) $ fromList [(2, 2), (1, 1), (3,4)]
map_3 = delete (13::Int) $ fromList [(10,0),(24,0),(13,0),(56,0),(35,0),(13,0),(6,0),(23,0)]

main = do
	print $ maybe (0) (id) (lookup 1 null_m)
	print $ maybe (Null) (id) (map_m)
	print $ maybe (Null) (id) (map_2)
	print $ map fst . toList $ maybe (Null) (id) (map_3)
	print $ fromList [(2, 2), (1, 1), (3,4)]
	print $ fromList [(10,0),(24,0),(13,0),(56,0),(35,0),(13,0),(6,0),(23,0)]