module Expr
    ( Value(..)
    , Expr(..)
    , UnOp(..)
    , BinOp(..)
    , Stat(..)
    , Prog(..)
    , priority
    ) where

import Text.PrettyPrint

data Value = I Integer | B Bool

data BinOp = Plus | Mul | Minus | And | Or | Less | Greater | Equals

data UnOp = Neg | Not

data Expr
    = Const Value
    | Var String
    | BinOp BinOp Expr Expr
    | UnOp UnOp Expr

newtype Prog = Prog [Stat]

data Stat
    = If Expr Stat Stat
    | While Expr Stat
    | Assign String Expr
    | Comp [Stat]

instance Show Value where
    show (I v) = show v
    show (B True) = "true"
    show (B False) = "false"

instance Show BinOp where
    show Plus = " + "
    show Mul = " * "
    show Minus = " - "
    show And = " && "
    show Or = " || "
    show Less = " < "
    show Greater = " > "
    show Equals = " == "

instance Show UnOp where
    show Neg = "-"
    show Not = "!"

priority And = 4
priority Or = 4
priority Less = 5
priority Greater = 5
priority Equals = 5
priority Plus = 6
priority Minus = 6
priority Mul = 7

instance Show Expr where
    showsPrec _ (Const v) = shows v
    showsPrec _ (Var x) = showString x
    showsPrec p (BinOp op e1 e2) = showParen (p > priority op) $
        showsPrec (priority op) e1 . shows op . showsPrec (priority op + 1) e2
    showsPrec p (UnOp op e) = shows op . showsPrec 10 e

instance Show Prog where
    show (Prog xs) = unlines (map show xs)

instance Show Stat where
    show = render . pretty
      where
        curly p b = vcat [p <+> lbrace, nest 4 b, rbrace]
        if_then c t = curly (text "if" <+> parens (text (show c))) (pretty t)
        pretty (If c t (Comp [])) = if_then c t
        pretty (If c t e) = curly (if_then c t <+> text "else") (pretty e)
        pretty (While c b) = curly (text "while" <+> parens (text (show c))) (pretty b)
        pretty (Assign v e) = text v <+> equals <+> text (show e) <> semi
        pretty (Comp ss) = vcat (map pretty ss)
