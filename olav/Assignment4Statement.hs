module Assignment4Statement

where

import Data.List
import Lecture4

instance Show Statement where
  show (Ass v e) =    v ++ " <- " ++ (show e)
  show (Cond c i e) = "if " ++ (show c) ++ (indent 2 ('\n':show i))
                        ++ "\nelse" ++ (indent 2 ('\n':show e))
  show (Seq []) =     ""
  show (Seq x) =      (intercalate ";\n" (map show x)) ++ ";"
  show (While c s) = "while " ++ (show c) ++ (indent 2 ('\n':(show s)))

indent :: Int -> String -> String
indent _ "" = ""
indent x (c:cs)
  | c == '\n' = c:(spaces x) ++ (indent x cs)
  | otherwise = c:(indent x cs)

spaces :: Int -> String
spaces 0 = ""
spaces x = " " ++ spaces (x-1)

instance Show Condition where
  show (Prp v) =    v
  show (Eq e1 e2) = (show e1) ++ " == " ++ (show e2)
  show (Lt e1 e2) = (show e1) ++ " < " ++ (show e2)
  show (Gt e1 e2) = (show e1) ++ ">" ++ (show e2)
  show (Ng c) =     "!(" ++ (show c) ++ ")"
  show (Cj c) =     intercalate "  & " (map show c)
  show (Dj c) =     intercalate "  | " (map show c)

instance Show Expr where
  show (I i) =         show i
  show (V v) =         v
  show (Add e1 e2) =   (show e1) ++ " + " ++ (show e2)
  show (Subtr e1 e2) = (show e1) ++ " - " ++ (show e2)
  show (Mult e1 e2) =  (show e1) ++ " * " ++ (show e2)