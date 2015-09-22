module Bonus1

where

type Var = String
type Env = Var -> Integer

data Expr = I Integer | V Var 
          | Add Expr Expr 
          | Subtr Expr Expr 
          | Mult Expr Expr 
          deriving (Eq)

data Condition = Prp Var 
               | Eq Expr Expr 
               | Lt Expr Expr 
               | Gt Expr Expr 
               | Ng Condition 
               | Cj [Condition] 
               | Dj [Condition]
               deriving (Eq)

data Statement = Ass Var Expr
               | Cond Condition Statement Statement
               | Seq [Statement]
               | While Condition Statement
               deriving (Eq)

instance Show Expr where
    show (I i)         = show i
    show (V n)         = show n
    show (Add e1 e2)   = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (Subtr e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
    show (Mult e1 e2)  = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

instance Show Condition where
    show (Prp n)    = show n
    show (Eq e1 e2) = show e1 ++ " == " ++ show e2
    show (Lt e1 e2) = show e1 ++ " < " ++ show e2
    show (Gt e1 e2) = show e1 ++ " > " ++ show e2
    show (Ng c)     = "!(" ++ show c ++ ")"
    show (Cj cs)    = "(" ++ showLst " && " cs ++ ")"
    show (Dj cs)    = "(" ++ showLst " || " cs ++ ")"

instance Show Statement where
    show (Ass v e)      = show v ++ " = " ++ show e ++ ";"
    show (Cond c s1 s2) = "if " ++ show c ++ " then " ++ show s1 ++ " else " ++ show s2

showLst,showRest :: String -> [Condition] -> String
showLst _ [] = ""
showLst s (f:fs) = show f ++ showRest s fs
showRest _ [] = ""
showRest s (f:fs) = s ++ show f ++ showRest s fs
