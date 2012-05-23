{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import Data.List

import Expr
import Parsing

type Store = M.Map String Value

type Error = String

undVar x = ["Undefined valriable '" ++ x ++ "'"]
typeError = ["Type error"]

evalExpr :: Store -> Expr -> Either [Error] Value
evalExpr _ (Const v) = Right v
evalExpr m (Var x) = maybe (Left (undVar x)) Right (M.lookup x m)
evalExpr m (UnOp Neg e) = case evalExpr m e of
    Left t -> Left t
    Right (I x) -> Right $ I (negate x)
    _ -> Left typeError
evalExpr m (UnOp Not e) = case evalExpr m e of
    Left t -> Left t
    Right (B x) -> Right $ B (not x)
    _ -> Left typeError
evalExpr m (BinOp op e1 e2) = case (evalExpr m e1, evalExpr m e2) of
    (Right v1, Right v2) -> evalBinop op v1 v2
    (Left t1, Left t2) -> Left (t1 `union` t2)
    (Left t, _) -> Left t
    (_, Left t) -> Left t
  where
    evalBinop :: BinOp -> Value -> Value -> Either [Error] Value
    evalBinop Plus (I arg1) (I arg2) = Right (I (arg1 + arg2))
    evalBinop Minus (I arg1) (I arg2) = Right (I (arg1 - arg2))
    evalBinop Mul (I arg1) (I arg2) = Right (I (arg1 * arg2))
    evalBinop And (B arg1) (B arg2) = Right (B (arg1 && arg2))
    evalBinop Or (B arg1) (B arg2) = Right (B (arg1 || arg2))
    evalBinop Less (I arg1) (I arg2) = Right (B (arg1 < arg2))
    evalBinop Greater (I arg1) (I arg2) = Right (B (arg1 > arg2))
    evalBinop Equals (I arg1) (I arg2) = Right (B (arg1 == arg2))
    evalBinop Equals (B arg1) (B arg2) = Right (B (arg1 == arg2))
    evalBinop _ _ _ = Left typeError

evalStat :: Store -> Stat -> ([Error], Store)
evalStat m (If c t e) = either (, m) right (evalExpr m c)
  where right (I _) = (typeError, m)
        right (B True) = evalStat m t
        right (B False) = evalStat m e
evalStat m (While c b) = either (, m) right (evalExpr m c)
  where right (I _) = (typeError, m)
        right (B True) = let
            (es, m') = evalStat m b
            (es', m'') = evalStat m' (While c b)
            in if null es then (es `union` es', m'') else (es, m')
        right (B False) = ([], m)
evalStat m (Assign v e) = either (, m) (\r -> ([], M.insert v r m)) (evalExpr m e)
evalStat m (Comp st) = evalProg m (Prog st)

evalProg :: Store -> Prog -> ([Error], Store)
evalProg m (Prog []) = ([], m)
evalProg m (Prog (s:ss)) = let
    (es, m') = evalStat m s
    (es', m'') = evalProg m' (Prog ss)
    in (es `union` es', m'')

inp1 = "!x || (y < 3 && z == y) && 5 < y + 7 * (z + y * 3)"
inp2 = "-5 + -3 * 2 - 7"
inp3 = "r = 1;\n"
    ++ "while (n > 0) {\n"
    ++ "\tr = r * n;\n"
    ++ "\tn = n - 1;\n"
    ++ "}"
inp4 = "if (x > 0) y = 1; else y = 2;"
inp5 = "if (x > 0) y = 1;"

main = do
    print $ If (Var "x") (Comp [Assign "y" (Var "x"), Assign "y" (Var "x")]) (Comp [Assign "y" (Var "x"), Assign "y" (Var "x")])
    print $ If (Var "x") (Comp [Assign "y" (Var "x"), Assign "y" (Var "x")]) (Comp [Assign "y" (Var "x")])
    print $ If (Var "x") (Comp [Assign "y" (Var "x"), Assign "y" (Var "x")]) (Comp [])
    print (read inp3 :: Prog)
    print (read inp4 :: Prog)
    print (read inp5 :: Prog)
    print . evalExpr (M.fromList [("x",B True),("t",I 5),("f",I 5)]) . read $ inp1
    print . evalExpr (M.fromList [("x",B True),("y",I 5),("z",I 5)]) . read $ inp1
    print . evalExpr (M.fromList [("x",B True),("y",I 2),("z",I 2)]) . read $ inp1
    print . evalExpr M.empty . read $ inp2
    print . evalProg (M.fromList [("n",I 5)]) . read $ inp3
