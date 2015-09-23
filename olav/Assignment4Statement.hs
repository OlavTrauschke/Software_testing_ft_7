module Assignment4Statement

where

import Data.Char
import Data.List
import Data.String
import Lecture4
import Prelude hiding (read)--Get rid of predifined read to implement it self

instance Show Statement where
  show (Ass v e) =    v ++ " = " ++ (show e) ++ ";"
  show (Cond c i e) = "if " ++ (show c) ++ " {" ++ (indent 2 ('\n':show i))
                        ++ "\n}" ++ "\nelse {" ++ (indent 2 ('\n':show e)) ++ "\n}"
  show (Seq []) =     ""
  show (Seq x) =      intercalate "\n" (map show x)
  show (While c s) = "while " ++ (show c) ++ " {" ++ (indent 2 ('\n':(show s))) ++ "\n}"

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
  show (Add e1 e2) =   "(" ++ (show e1) ++ " + " ++ (show e2) ++ ")"
  show (Subtr e1 e2) = "(" ++ (show e1) ++ " - " ++ (show e2) ++ ")"
  show (Mult e1 e2) =  "(" ++ (show e1) ++ " * " ++ (show e2) ++ ")"

--read :: String -> Statement
--read = parse.tokenize

data Token = AssToken | EOSToken | IfToken | ElseToken | WhileToken | OBToken | CBToken
           | OCBToken | CCBToken | EqToken | LTToken | GTToken | NgToken | CjToken
           | DjToken | IntToken Integer | VToken String | AddToken | SubtrToken | MultToken
           deriving (Show)

tokenize :: String -> [Token]
tokenize [] =                             []
tokenize (' ':xs) =                       tokenize xs
tokenize ('\n':xs) =                      tokenize xs
tokenize (';':xs) =                       EOSToken:tokenize xs
tokenize ('i':'f':' ':xs) =               IfToken:tokenize xs
tokenize ('e':'l':'s':'e':' ':xs) =       ElseToken:tokenize xs
tokenize ('w':'h':'i':'l':'e':' ':xs) =   WhileToken:tokenize xs
tokenize ('(':xs) =                       OBToken:tokenize xs
tokenize (')':xs) =                       CBToken:tokenize xs
tokenize ('{':xs) =                       OCBToken:tokenize xs
tokenize ('}':xs) =                       CCBToken:tokenize xs
tokenize ('=':'=':xs) =                   EqToken:tokenize xs
tokenize ('=':xs) =                       AssToken:tokenize xs
tokenize ('<':xs) =                       LTToken:tokenize xs
tokenize ('>':xs) =                       GTToken:tokenize xs
tokenize ('!':xs) =                       NgToken:tokenize xs
tokenize ('&':xs) =                       CjToken:tokenize xs
tokenize ('|':xs) =                       DjToken:tokenize xs
tokenize ('+':xs) =                       AddToken:tokenize xs
--dash for substraction should always be followed by a space
tokenize ('-':' ':xs) =                   SubtrToken:tokenize xs
tokenize ('*':xs) =                       MultToken:tokenize xs
tokenize ('-':xs) =                       (IntToken (stringToInteger n)):tokenize (xs\\n)
  where n = '-':getNumbers xs
tokenize (x:xs)
  | isDigit x =                           (IntToken (stringToInteger n)):tokenize (xs\\n)
    --variable names should start with a letter and can not equal "if", "else" or "while"
  | isLetter x =                          (VToken v):tokenize (xs\\v) 
    where n = getNumbers (x:xs)
          v = getChars (x:xs)

getNumbers :: String -> String
getNumbers [] = []
getNumbers (x:xs)
  | isDigit x = x:getNumbers xs
  | otherwise = []

getChars :: String -> String
getChars [] = []
getChars (' ':xs) = []
getChars (x:xs) = x:getChars xs

stringToInteger :: String -> Integer
stringToInteger [] = 0
stringToInteger ('-':xs) = -(stringToInteger xs)
stringToInteger (x:xs) = (toInteger ((digitToInt x))*10^(length xs))+(stringToInteger xs)

--parse :: [Token] -> Statement
--parse (AssToken:(VToken v):e:EOSToken) = Ass v (parse e)
--parse