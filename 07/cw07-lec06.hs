{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Char

data Tree a b = Leaf b | Node (Tree a b) (Tree a b) a 

instance (Eq a, Eq b) => Eq (Tree a b) where
	(==) (Node l1 r1 v1) (Node l2 r2 v2) = v1 == v2 && l1 == l2 && r1 == r2
	(==) (Leaf v1) (Leaf v2) = v1 == v2
	(==) _ _ = False

leaf1 = (Leaf 1::Tree Int Int)
leaf2 = (Leaf 2::Tree Int Int)
node1 = (Node leaf1 leaf1 1)
node2 = (Node leaf1 leaf2 1)

--tests for Eq Tree
leafleaf_f = leaf1 == leaf2
leafleaf_t = leaf1 == leaf1
leafnode_f = leaf1 == node1
nodenode_t = node1 == node1
nodenode_f = node1 == node2

-----
instance (Show a, Show b) => Show (Tree a b) where
	--show (Leaf v) = show v
	--show (Node l r v) = "<" ++ show l ++ "{" ++ show v ++ "}" ++ show r ++ ">"
	showsPrec _ (Leaf v) = shows v
	showsPrec _ (Node l r v) = (showString "<") . shows l . 
		(showString "{") . shows v . (showString "}") . shows r . (showString ">")

tree = (Node
			(Leaf 2)
			(Node
				(Leaf 4)
				(Leaf 5)
				3)
			
			1)

---
class Conv a b where
	conv :: a -> b

instance Conv Int Double where
	conv = fromIntegral

instance Conv Double Int where
	conv = truncate 

instance Conv Int Char where
	conv = chr

instance Conv Char Int where
	conv = ord

--(Maybe a) [a]
instance Conv (Maybe a) [a] where
	conv (Just x) = [x]
	conv _ = []

--(Maybe a) [Bool]
instance Conv (Maybe a) Bool where
	conv (Just _) = True
	conv _ = False

--[a] [b]
instance (Conv a b) => Conv [a] [b] where
	conv = map conv

--(Maybe a) (Maybe b)
instance (Conv a b) => Conv (Maybe a) (Maybe b) where
	conv = fmap conv

--(a, b)   (c, d)
--(a->b)   (c->d)

main = print $ (conv (Nothing::(Maybe Char))::(Maybe Int))
