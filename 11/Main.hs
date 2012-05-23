import qualified Data.Map as M
import Control.Applicative hiding (Const)
import Control.Monad

import Eval
import Expr
import Parsing

if' :: Eval Value -> Eval a -> Eval a -> Eval a
if' = undefined

evalExpr :: Expr -> Eval Value
evalExpr = undefined

evalStat :: Stat -> Eval ()
evalStat = undefined

evalProg :: Prog -> Eval ()
evalProg = undefined

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
    print . ($ M.fromList [("x",B True),("t",I 5),("f",I 5)]) . runEval . evalExpr . read $ inp1
    print . ($ M.fromList [("x",B True),("y",I 5),("z",I 5)]) . runEval . evalExpr . read $ inp1
    print . ($ M.fromList [("x",B True),("y",I 2),("z",I 2)]) . runEval . evalExpr . read $ inp1
    print . ($ M.empty) . runEval . evalExpr . read $ inp2
    print . ($ M.fromList [("n",I 5)]) . runEval . evalProg . read $ inp3
