import qualified Data.Map as M
import Debug.Trace

import Expr
import Parsing

type Store = M.Map String Value

type Error = String

--4. В этом задании необходимо написать интерпретатор для простого языка с си подобным синтаксисом.
--Я описал все необходимые типы данных, они находятся в файле Expr.hs. Необходимо разобраться с этими определениями.
--Также я написал парсер для него, он находится в файле Parsing.hs. Разбираться в нем не обязательно, но можно. Только учтите, что я там использовал несколько вещей, которые мы еще не проходили, а некоторые и не пройдем.
--В файле Main.hs лежит несколько примеров. Также там находятся функции evalExpr, evalStat и evalProg, которые вам необходимо реализовать.
--type Store - это мап, который хранит текущее значение переменных.
--Функция evalExpr возвращает либо вычисленное значение выражения, либо список ошибок. Ошибки бывают двух видов: неизвестная переменная и несоответствие типов.
--Функции evalStat и evalProg возвращают список ошибок (если ошибок не было, список пустой) и обновленный Store.

--Для работы моего кода вам понадобится библиотека pretty. Устанавливается она так: cabal install pretty.

evalExpr :: Store -> Expr -> Either [Error] Value
evalExpr _ (Const value) = Right value
evalExpr store (Var var) = maybe (Left ["Undefined variable '" ++ var ++ "'"]) Right (M.lookup var store)

evalExpr store (UnOp Neg exp) = case (evalExpr store exp) of
                                    Right (I val)   -> Right $ I (-val)
                                    r@(Left errors) -> r
                                    _               -> Left ["Wrong type"]
evalExpr store (UnOp Not exp) = case (evalExpr store exp) of
                                    Right (B val)   -> Right . B $ not val
                                    r@(Left errors) -> r
                                    _               -> Left ["Wrong type"]

evalExpr store (BinOp Plus exp1 exp2) = evalIntBinOp store (+) exp1 exp2
evalExpr store (BinOp Minus exp1 exp2) = evalIntBinOp store (-) exp1 exp2
evalExpr store (BinOp Mul exp1 exp2) = evalIntBinOp store (*) exp1 exp2
evalExpr store (BinOp Less exp1 exp2) = evalCmpBinOp store (<) exp1 exp2
evalExpr store (BinOp Greater exp1 exp2) = evalCmpBinOp store (>) exp1 exp2

evalExpr store (BinOp And exp1 exp2) = evalBoolBinOp store (&&) exp1 exp2
evalExpr store (BinOp Or exp1 exp2) = evalBoolBinOp store (||) exp1 exp2

evalExpr store (BinOp Equals exp1 exp2) = case (evalExpr store exp1) of
                Right (I val1)  -> 
                    case (evalExpr store exp2) of
                        Right (I val2)  -> Right $ B (val1 == val2)
                        r@(Left errors) -> r
                        _               -> Left ["Wrong type"]
                Right (B val1)  -> 
                    case (evalExpr store exp2) of
                        Right (B val2)  -> Right $ B (val1 == val2)
                        r@(Left errors) -> r
                        _               -> Left ["Wrong type"]
                errors          -> errors

evalIntBinOp store op exp1 exp2 = case (evalExpr store exp1) of
                Right (I val1)  -> 
                    case (evalExpr store exp2) of
                        Right (I val2)  -> Right $ I (val1 `op` val2)
                        r@(Left errors) -> r
                        _               -> Left ["Wrong type"]
                r@(Left errors) -> r
                _               -> Left ["Wrong type"]

evalCmpBinOp store op exp1 exp2 = case (evalExpr store exp1) of
                Right (I val1)  -> 
                    case (evalExpr store exp2) of
                        Right (I val2)  -> Right $ B (val1 `op` val2)
                        r@(Left errors) -> r
                        _               -> Left ["Wrong type"]
                r@(Left errors) -> r
                _               -> Left ["Wrong type"]

evalBoolBinOp store op exp1 exp2 = case (evalExpr store exp1) of
                Right (B val1)  -> 
                    case (evalExpr store exp2) of
                        Right (B val2)  -> Right $ B (val1 `op` val2)
                        r@(Left errors) -> r
                        _               -> Left ["Wrong type"]
                r@(Left errors) -> r
                _               -> Left ["Wrong type"]

evalStat :: Store -> Stat -> ([Error], Store)
evalStat store (If cond thenStat elseStat) = case (evalExpr store cond) of
                                                Right (B v)   -> evalStat store (if v then thenStat else elseStat)
                                                (Left errors) -> (errors, store)
                                                _             -> (["Wrong type"], store)

evalStat store w@(While cond stat) = case (evalExpr store cond) of
                                        Right (B v)   -> 
                                            if v then
                                                case (evalStat store stat) of
                                                    ([], newStore) -> evalStat newStore w
                                                    r              -> r
                                            else
                                                ([], store)
                                        (Left errors) -> (errors, store)
                                        _             -> (["Wrong type"], store)


evalStat store (Assign var exp) = case (evalExpr store exp) of
    Right val   -> ([], M.insert var val store)
    Left errors -> (errors, store)

evalStat store (Comp []) = ([], store)
evalStat store (Comp (stat:ss)) = case (evalStat store stat) of
    ([], newStore) -> evalStat newStore (Comp ss)
    r              -> r

evalProg :: Store -> Prog -> ([Error], Store)
evalProg store (Prog stats) = evalStat store (Comp stats)

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
    print (read inp3 :: Prog)
    print (read inp4 :: Prog)
    print (read inp5 :: Prog)
    print . evalExpr (M.fromList [("x",B True),("t",I 5),("f",I 5)]) . read $ inp1
    print . evalExpr (M.fromList [("x",B True),("y",I 5),("z",I 5)]) . read $ inp1
    print . evalExpr (M.fromList [("x",B True),("y",I 2),("z",I 2)]) . read $ inp1
    print . evalExpr M.empty . read $ inp2
    print . evalProg (M.fromList [("n",I 5)]) . read $ inp3
