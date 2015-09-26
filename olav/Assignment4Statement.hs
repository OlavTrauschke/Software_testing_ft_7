module Assignment4Statement

where

import Control.Monad
import Data.Char
import Data.List
import Data.String
import Lecture4
import Prelude hiding (read)--Get rid of predifined read to implement it self
import Test.QuickCheck

instance Show Statement where
  show (Ass v e) =                v ++ " = " ++ show e ++ ";"
  show (Cond c i e) =             "if " ++ show c ++ " " ++ show i ++ "\nelse" ++ " "
                                    ++ show e
  show (Seq []) =                 "{}"
  show (Seq x) =                  "{" ++ indent ('\n':intercalate "\n" (map show x))
                                    ++ "\n}"
  show (While c s) =              "while " ++ show c ++ " " ++ show s

indent :: String -> String
indent "" = ""
indent "\n}" = "\n}"
indent ('\n':cs) = "\n  " ++ indent cs
indent (c:cs) =    c:indent cs

instance Show Condition where
  show (Prp v) =    v
  show (Eq e1 e2) = bracket (show e1 ++ " == " ++ show e2)
  show (Lt e1 e2) = bracket (show e1 ++ " < " ++ show e2)
  show (Gt e1 e2) = bracket (show e1 ++ " > " ++ show e2)
  show (Ng c) =     "!" ++ show c
  show (Cj c) =     bracket (intercalate "  & " (map show c))
  show (Dj c) =     bracket (intercalate "  | " (map show c))

bracket :: String -> String
bracket x = '(':x ++ ")"

instance Show Expr where
  show (I i) =         show i
  show (V v) =         v
  show (Add e1 e2) =   "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Subtr e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Mult e1 e2) =  "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

read :: String -> Statement
read = parse.tokenize

--lexical analysis
data Token = AssToken | EOSToken | IfToken | ElseToken | WhileToken | OBToken | CBToken
           | OCBToken | CCBToken | EqToken | LTToken | GTToken | NgToken | CjToken
           | DjToken | IntToken Integer | VToken String | AddToken | SubtrToken | MultToken
           deriving (Show,Eq)

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
tokenize ('-':' ':xs) =                   SubtrToken:tokenize xs
tokenize ('*':xs) =                       MultToken:tokenize xs
tokenize ('-':xs) =                       IntToken (stringToInteger ('-':i))
                                            :tokenize (xs\\i)
  where i = getNumbers xs
tokenize (x:xs)
  | isDigit x = let
                  i = getNumbers (x:xs)
                in (IntToken (stringToInteger i)):(tokenize ((x:xs)\\i))
  | isLetter x = let
                   v = getChars (x:xs)
                 in (VToken v):(tokenize ((x:xs)\\v))

getNumbers :: String -> String
getNumbers [] = []
getNumbers (x:xs)
  | isDigit x = x:getNumbers xs
  | otherwise = []

getChars :: String -> String
getChars [] = []
getChars (x:xs) 
  | elem x [' ',';',')','}'] = []
  | otherwise = x:getChars xs

stringToInteger :: String -> Integer
stringToInteger [] = 0
stringToInteger ('-':xs) = -stringToInteger xs
stringToInteger (x:xs) = toInteger (digitToInt x*10^length xs)+stringToInteger xs

--parsing
parse :: [Token] -> Statement
parse (VToken v:AssToken:xs) = Ass v (parseExpr e)
  where e = getExpression xs
parse (IfToken:xs) = Cond (parseCond c) (parse i) (parse e)
  where c = getCondition xs
        i = getStatement (xs\\c)
        e = getStatement (delete ElseToken ((xs\\c)\\i))
parse (OCBToken:xs) = Seq (toListOfStatements (init xs))
parse (WhileToken:xs) = While (parseCond c) (parse s)
  where c = getCondition xs
        s = getStatement (xs\\c)

parseExpr :: [Token] -> Expr
parseExpr ((IntToken i):xs) = I i
parseExpr ((VToken v):xs) = V v
parseExpr (OBToken:xs) = parseOp op (parseExpr o1) (parseExpr o2)
  where o1 = getExpression xs
        op = head (xs\\o1)
        o2 = getExpression (delete op (xs\\o1))

parseOp :: Token -> (Expr -> Expr -> Expr)
parseOp AddToken = Add
parseOp SubtrToken = Subtr
parseOp MultToken = Mult

--get the smallest prefix of a list of Tokens that represent a complete statement
getExpression :: [Token] -> [Token]
getExpression (IntToken i:xs) = [IntToken i]
getExpression (VToken v:xs) = [VToken v]
getExpression (OBToken:xs) = OBToken:getUntilMatching 0 OBToken CBToken xs
getExpression (NgToken:xs) = NgToken:getExpression xs

--get the smallest prefix of a list of tokens containg n+1 more occurences of target
--than of inverseTarget
getUntilMatching :: Integer -> Token -> Token -> [Token] -> [Token]
getUntilMatching n inverseTarget target (t:ts)
  | n == 0 && t == target = [t]
  | t == target = t:getUntilMatching (n-1) inverseTarget target ts
  | t == inverseTarget = t:getUntilMatching (n+1) inverseTarget target ts
  | otherwise = t:getUntilMatching n inverseTarget target ts

parseCond :: [Token] -> Condition
parseCond (VToken v:xs) = Prp v
parseCond (NgToken:xs) = Ng (parseCond xs)
parseCond (OBToken:xs)
  | cOp == CBToken = parseCond o1
  | cOp == CjToken || cOp == DjToken = parseListCOp cOp (getConditionList cOp xs)
  | otherwise = let
                  o2 = getExpression (delete cOp (xs\\o1))
                in parseCOp cOp (parseExpr o1) (parseExpr o2)
  where o1 = getExpression xs
        cOp = head (xs\\o1)

--get the smallest prefix of a list of Tokens that represent a complete condition
getCondition :: [Token] -> [Token]
getCondition (VToken v:xs) = [VToken v]
getCondition (NgToken:xs) = NgToken:getCondition xs
getCondition (OBToken:xs) = OBToken:getUntilMatching 0 OBToken CBToken xs

--get a list of conditions from a list of Tokens representing them seperated by a
--given token representing the operation that is connecting them
getConditionList :: Token -> [Token] -> [Condition]
getConditionList op (x:xs)
  | x == op = getConditionList op xs
  | x == CBToken = []
  | otherwise = let
                  c = getCondition (x:xs)
                in parseCond c:getConditionList op ((x:xs)\\c)                 

parseListCOp :: Token -> [Condition] -> Condition
parseListCOp CjToken = Cj
parseListCOp DjToken = Dj

parseCOp :: Token -> Expr -> Expr -> Condition
parseCOp EqToken = Eq
parseCOp LTToken = Lt
parseCOp GTToken = Gt

--get the smallest prefix of a list of Tokens that represents a complete statement
getStatement :: [Token] -> [Token]
getStatement (VToken v:AssToken:xs) = VToken v:AssToken:getUntil EOSToken xs
getStatement (IfToken:xs) = IfToken:c ++ i ++ ElseToken:e
  where c = getCondition xs
        i = getStatement (xs\\c)
        e = getStatement (delete ElseToken ((xs\\c)\\i))
getStatement (OCBToken:xs) = OCBToken:getUntilMatching 0 OCBToken CCBToken xs
getStatement (WhileToken:xs) = WhileToken:c ++ s
  where c = getCondition xs
        s = getStatement (xs\\c)

--get the smallest prefix of a list of Tokens ending with a given Token
getUntil :: Token -> [Token] -> [Token]
getUntil target (x:xs)
  | x == target = [x]
  | otherwise = x:getUntil target xs

--convert a list of Tokens into the list of statements they represent
toListOfStatements :: [Token] -> [Statement]
toListOfStatements [] = []
toListOfStatements x = parse s:toListOfStatements (x\\s)
  where s = getStatement x

--property expressing that reading the result of showing a statement gives that statements,
--for testing read and show using quickCheck
prop_readInverseOfShow :: Statement -> Bool
prop_readInverseOfShow x = x == (read.show) x

--aribtrary generators for using quickCheck to test read and show
instance Arbitrary Statement where
  arbitrary = sized arbStatement
  shrink (Ass v e) =      []
  shrink (Cond c s1 s2) = s1:[s2]
  shrink (Seq s) =        s
  shrink (While c s) =    [s]

arbStatement :: Int -> Gen Statement
arbStatement 0 = liftM2 Ass  arbVar (arbExpr 0)
arbStatement n = oneof [liftM2 Ass  arbVar (arbExpr m),
                        liftM3 Cond (arbCondition m) (arbStatement m) (arbStatement m),
                        liftM  Seq  (arbStatementList m),
                        liftM2 While (arbCondition m) (arbStatement m)]
  where m = n - 1

arbStatementList :: Int -> Gen [Statement]
arbStatementList 0 = liftM2 (:) (arbStatement 0) (return [])
arbStatementList n = liftM2 (:) (arbStatement m) (arbStatementList m)
  where m = n - 1

arbExpr :: Int -> Gen Expr
arbExpr 0 = oneof [liftM I arbitrary,
                   liftM V arbVar]
arbExpr n = oneof [liftM  I     arbitrary,
                   liftM  V     arbVar,
                   liftM2 Add   (arbExpr m) (arbExpr m),
                   liftM2 Subtr (arbExpr m) (arbExpr m),
                   liftM2 Mult  (arbExpr m) (arbExpr m)]
  where m = n - 1

arbCondition :: Int -> Gen Condition
arbCondition 0 = liftM Prp arbVar
arbCondition n = oneof [liftM  Prp arbVar,
                        liftM2 Eq  (arbExpr m) (arbExpr m),
                        liftM2 Lt  (arbExpr m) (arbExpr m),
                        liftM2 Gt  (arbExpr m) (arbExpr m),
                        liftM  Ng  (arbCondition m),
                        liftM  Cj  (arbConditionList m),
                        liftM  Dj  (arbConditionList m)]
  where m = n - 1

arbConditionList :: Int -> Gen [Condition]
--No conjunctions or disjunctions of less than two elements are allowed, because they would
--not be recognizable as conjunction or disjunction
arbConditionList 0 = liftM2 (:) (arbCondition 0) (liftM2 (:) (arbCondition 0) (return []))
arbConditionList n = liftM2 (:) (arbCondition m) (arbConditionList m)
  where m = n - 1

{-generator for arbitrary Variable names, based on Ganesh Sittampalam's at
http://stackoverflow.com/questions/20934506
/haskell-quickcheck-how-to-generate-only-printable-strings from Jan 5 '14 17:09, edited
on Jan 6 '14 at 6:55 as found on Sep 26 '15-}
arbVar :: Gen String
arbVar = (listOf1.elements) (['a'..'z'] ++ ['A'..'Z'])