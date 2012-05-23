module Parsing() where

import Control.Arrow
import Control.Monad
import Data.Char

import Expr

instance Read Value where
    readsPrec _ ('t':'r':'u':'e':xs) = [(B True, xs)]
    readsPrec _ ('f':'a':'l':'s':'e':xs) = [(B False, xs)]
    readsPrec p xs = map (first I) (readsPrec p xs)

instance Read UnOp where
    readsPrec _ ('-':xs) = [(Neg, xs)]
    readsPrec _ ('!':xs) = [(Not, xs)]
    readsPrec _ _ = []

instance Read BinOp where
    readsPrec _ ('+':xs) = [(Plus, xs)]
    readsPrec _ ('-':xs) = [(Minus, xs)]
    readsPrec _ ('*':xs) = [(Mul, xs)]
    readsPrec _ ('&':'&':xs) = [(And, xs)]
    readsPrec _ ('|':'|':xs) = [(Or, xs)]
    readsPrec _ ('<':xs) = [(Less, xs)]
    readsPrec _ ('>':xs) = [(Greater, xs)]
    readsPrec _ ('=':'=':xs) = [(Equals, xs)]
    readsPrec _ _ = []

data Tree = Branch Tree [(BinOp, Tree)] | Leaf Expr

lexemes :: String -> [String]
lexemes "" = []
lexemes xs = case lex xs of
    [] -> []
    ("", _):_ -> []
    (l, xs'):_ -> l : lexemes xs'

syn :: [String] -> Maybe (Tree, [String])
syn ("(":xs) = case syn xs of
    Just (t, ")":xs') -> syn1 t xs'
    _ -> Nothing
syn (x:xs) | (op, ""):_ <- reads x = fmap (first $ unary op) (syn xs)
  where unary op (Leaf e) = Leaf (UnOp op e)
        unary op (Branch t ts) = Branch (unary op t) ts
syn (x:xs) | (v, ""):_ <- reads x = syn1 (Leaf $ Const v) xs
syn (x@(c:_):xs) | isAlpha c = syn1 (Leaf $ Var x) xs
syn _ = Nothing

syn1 :: Tree -> [String] -> Maybe (Tree, [String])
syn1 t (")":xs) = Just (t, ")":xs)
syn1 t (x:xs) | (op, ""):_ <- reads x = fmap (first $ binary op) (syn xs)
  where binary op (Branch t' ts) = Branch t $ (op,t'):ts
        binary op t' = Branch t [(op, t')]
syn1 t xs = Just (t, xs)

tree2expr :: Tree -> Expr
tree2expr (Leaf e) = e
tree2expr (Branch t ts) = exprs2expr [] (tree2expr t) ts
  where
    exprs2expr [] e [] = e
    exprs2expr ((op,e'):st) e [] = exprs2expr st (BinOp op e' e) []
    exprs2expr [] e ((op,e'):es) = exprs2expr [(op,e)] (tree2expr e') es
    exprs2expr st'@((op1,e1):st) e es'@((op2,e2):es)
        | priority op2 > priority op1 = exprs2expr ((op2,e):st') (tree2expr e2) es
        | otherwise = exprs2expr st (BinOp op1 e1 e) es'

instance Read Expr where
    readsPrec _ = maybe [] (return . (tree2expr *** unwords)) . syn . lexemes

instance Read Prog where
    readsPrec _ xs = do (st, xs) <- reads xs
                        (Prog pr, xs) <- reads xs
                        return (Prog (st:pr), xs)
                     `mplus` return (Prog [], xs)

instance Read Stat where
    readsPrec _ xs = do
        (l, xs) <- lex xs
        case l of
            "if" -> do (c, xs) <- reads xs
                       (t, xs) <- reads xs
                       do ("else", xs) <- lex xs
                          (e, xs) <- reads xs
                          return (If c t e, xs)
                          `mplus` return (If c t (Comp []), xs)
            "while" -> do (c, xs) <- reads xs
                          (b, xs) <- reads xs
                          return (While c b, xs)
            "{" -> do (Prog r, xs) <- reads xs
                      ("}", xs) <- lex xs
                      return (Comp r, xs)
            c:_ | isAlpha c -> do ("=", xs) <- lex xs
                                  (e, xs) <- reads xs
                                  (";", xs) <- lex xs
                                  return (Assign l e, xs)
            _ -> []
