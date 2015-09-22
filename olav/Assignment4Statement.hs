module Assignment4Statement

where

import Data.List
import Lecture4

instance Show Statement where
  show (Ass v e) =    (show v) ++ " <- " ++ (show e)
  show (Cond c i e) = "if " ++ (show c) ++ "\n  then" ++ (indent 4 ('\n':show i))
                        ++ "\n  else" ++ (indent 4 ('\n':show e))
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