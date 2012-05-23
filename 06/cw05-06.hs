--main = print $ reverse' "1234567"

reverse' xs = reverse'' xs []
reverse'' [] ys = ys
reverse'' (x:xs) ys = reverse'' xs (x:ys)

--2012-03-20--
type D = Double
data Point = Point {x :: Double, y :: Double}

f (Point x y) = (x, y)

g :: Point -> (D, D)
g p = (x p, y p)
h (Point a b) = (a, b)

--Maybe
--Either

--Left "" :: Either String b

data List a = Nil | Cons a (List a)
--     [a]  =  [] | (:) a ([a])

data Tree a b = Leaf b | Node (Tree a b) (Tree a b) a 

data LambdaTerm = Variable String | Apply LambdaTerm LambdaTerm | Lambda String LambdaTerm

k::LambdaTerm
k = Lambda "x" (Lambda "y" (Variable "x"))


--глубина
depth :: Tree a b -> Int
depth (Leaf _) = 1
depth (Node l r _) = 1 + max (depth l) (depth r)

avgTree :: Tree Int Int -> Int
avgTree t = ()
	where
		avgTree' t s c = 

--ширина
tree = (Node 
			(Node
				(Node
					(Leaf 1)
					(Leaf 1)
					3)
				(Leaf 2)
				4)
			(Leaf 3)
			5)


main = print $ depth tree

