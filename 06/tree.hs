data Tree a b = Branch (Tree a b) a (Tree a b) | Leaf b

-- Возвращает высоту дерева
height :: Tree a b -> Int
height (Leaf _) = 0
height (Branch l _ r) = 1 + max (height l) (height r)

-- Возвращает среднее арифметическое значений во всех узлах дерева
-- Необходимо вычислить эту функцию, выполнив один проход по дереву
avg :: Tree Int Int -> Int
avg t = uncurry div (sum' t)
   where
       sum' (Leaf v) = (v, 1)
       sum' (Branch l v r) = let
           (ls, lc) = sum' l
           (rs, rc) = sum' r
           in (v + ls + rs, 1 + lc + rc)

-- Возвращает ширину дерева
-- Ширина дерева определяется следующим образом:
-- Количество вершин на определенном уровне называется шириной уровня.
-- Ширина дерева - это максимальная ширина уровня по всем уровням.
width :: Tree a b -> Int
width t = width' [t]
	where
		width' [] = 0
		width' st = max (length st) (width' $ nextGen st)
			where
				nextGen [] = []
				nextGen ((Leaf _) : xs) = (nextGen xs)
				nextGen ((Branch l _ r) : xs) = l : r : (nextGen xs)

width2 t = maximum $ width' t
	where
		width' (Leaf _) = [1]
		width' (Branch l v r) = 1 : (zipWith' (+) (width' l) (width' r))
			where
				zipWith' f xs [] = xs
				zipWith' f [] xs = xs
				zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

tree = b(b (b l l) (b l (b l l))) (b l (b l (b l l)))
  where l = Leaf 500; b l r = Branch l 300 r

main = do
    print (height tree) -- 4
    print (avg tree) -- 405
    print (width tree) -- 6
    print (width2 tree) -- 6
