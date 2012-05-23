import Debug.Trace
--Определить тип данных для представления чистых нетипизированных лямбда-термов. Реализовать функцию norm :: Term -> Term, возвращающую нормальную форму терма.

data LambdaTerm = Variable String | Apply LambdaTerm LambdaTerm | Lambda String LambdaTerm deriving Show

norm :: LambdaTerm -> LambdaTerm
norm (Apply l r) = apply l r
	where 
		apply (Lambda v l) t = replace v t l
		apply (Apply l r) t = norm $ apply (apply l r) t
		apply v@(Variable _) t = Apply v (norm t)
		replace v t (Variable x) | v == x = t
		replace v t@(Variable tv) (Lambda a l) | v /= a =
			if tv /= a
				then Lambda a  (replace v t l)
				else Lambda a' (replace v t (replace a (Variable a') l))
					where
						a'= (a++"`")
		replace v t (Lambda a l) | v /= a = Lambda a (replace v t l)
		replace v t (Apply l@(Variable _) r) = Apply (replace v t l) (norm (replace v t r))
		replace v t (Apply l r) = replace v t (apply l r)
		replace _ _ l = l

norm (Lambda s l) = Lambda s (norm l)
norm (Variable v) = Variable v

-----------------
f1 = Apply (Lambda "y" (Variable "y")) (Variable "w")
f2 = Apply (Apply k (Variable "z")) (Variable "w")
f3 = Apply k (Variable "y")
f4 = Lambda "x" (Apply k (Variable "r"))
f5 = Apply (Variable "x") (Variable "x")
--(x (\y. y))
f6 = Apply (Variable "x") (Lambda "y" (Variable "y"))
--(x ((\y. y) z)) -> (x z)
f7 = Apply (Variable "x") (Apply (Lambda "y" (Variable "y")) (Variable "z"))
f8 = Apply (Lambda "x" (Lambda "y" (Apply (Lambda "y`" (Apply (Variable "y") (Variable "x"))) (Variable "y")))) (Variable "y")
--(Lambda "y`" (Apply (Variable "y`") (Variable "y")))
--(\t. t t) ((\x. x x) (\y z. y z)) --> (\t. t t) ((\y z. y z) (\y z. y z)) -> (\t. t t) (\z. (\y z. y z) z) -> (\t. t t) (\z z'. z z') -> (\z z'. z z') (\z z'. z z') -> (\z'. (\z z'. z z') z') -> (\z' z''. z' z'')
f9 = Apply
		(Lambda "t" (Apply (Variable "t") (Variable "t"))) 
		(Apply 
			(Lambda "x" (Apply (Variable "x") (Variable "x"))) 
			(Lambda "y" (Lambda "z" (Apply (Variable "y") (Variable "z")))))

 --(\x y. x y) (y z)
f10 = Apply (Lambda "x" (Lambda "y" (Apply (Variable "x") (Variable "y")))) (Apply (Variable "y") (Variable "z"))

k :: LambdaTerm
k = Lambda "x" (Lambda "y" (Variable "x"))

main = do
	--print $ f1
	--print $ norm f1
	--print $ f2
	--print $ norm f2
	--print $ f3
	--print $ norm f3
	--print $ f4
	--print $ norm f4
	--print $ f5
	--print $ norm f5
	--print $ f6
	--print $ norm f6
	--print $ f7
	--print $ norm f7
	--print $ f8
	--print $ norm f8
	--print $ f9
	--print $ norm f9
	print $ f10
	print $ norm f10
