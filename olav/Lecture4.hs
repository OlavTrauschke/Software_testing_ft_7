module Lecture4

where

import Data.List
import Data.Char
import Test.QuickCheck

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (x,y) = \ z -> if x == z then y else f z

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
updates = foldl update

infixl 1 $$
 
($$) :: a -> (a -> b) -> b
($$) = flip ($)

type Var = String
type Env = Var -> Integer

data Expr = I Integer | V Var
          | Add Expr Expr
          | Subtr Expr Expr
          | Mult Expr Expr
          deriving (Eq)

eval :: Env -> Expr -> Integer
eval _ (I i) = i
eval c (V name) = c name
eval c (Add e1 e2)   = (eval c e1) + (eval c e2)
eval c (Subtr e1 e2) = (eval c e1) - (eval c e2)
eval c (Mult e1 e2)  = (eval c e1) * (eval c e2)

assign :: Var -> Expr -> Env -> Env
assign var expr c = let
   value = eval c expr
 in
   update c (var,value)

initEnv :: Env
initEnv = \ _ -> undefined

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

evalc :: Env -> Condition -> Bool
evalc env (Prp v)    = env v /= 0
evalc env (Eq e1 e2) = eval env e1 == eval env e2
evalc env (Lt e1 e2) = eval env e1 <  eval env e2
evalc env (Gt e1 e2) = eval env e1 >  eval env e2
evalc env (Ng c) = not (evalc env c)
evalc env (Cj cs) = and (map (evalc env) cs)
evalc env (Dj cs) = or  (map (evalc env) cs)

exec :: Env -> Statement -> Env
exec env (Ass v e) = assign v e env
exec env (Cond c s1 s2) =
 if evalc env c then exec env s1 else exec env s2
exec env (Seq ss) = foldl exec env ss
exec env w@(While c s) =
 if not (evalc env c) then env
 else exec (exec env s) w

fib :: Statement
fib = Seq [Ass "x" (I 0), Ass "y" (I 1),
           While (Gt (V "n") (I 0))
             (Seq [Ass "z" (V "x"), 
                   Ass "x" (V "y"),
                   Ass "y" (Add (V "z") (V "y")),
                   Ass "n" (Subtr (V "n") (I 1))])]

run :: [(Var,Integer)] -> Statement -> [Var] -> [Integer]
run xs program vars =
  exec (updates initEnv xs) program $$
    \ env -> map (eval env) (map V vars)

runFib n = run [("n",n)] fib ["x"]

while :: (a -> Bool) -> (a -> a) -> a -> a
while = until . (not.)

whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
whiler p f r = r . while p f

fibonacci :: Integer -> Integer
fibonacci n = fibon (0,1,n) where
  fibon = whiler
           (\ (_,_,n) -> n > 0)
           (\ (x,y,n) -> (y,x+y,n-1))
           (\ (x,_,_) -> x)

fp :: Eq a => (a -> a) -> a -> a 
fp f = until (\ x -> x == f x) f

fbo n = (0,1,n) $$
         fp (\ (x,y,k) -> if k == 0 then (x,y,k) else (y,x+y,k-1))

bab a = \ x -> ((x + a/x)/2)

sr a = fp (bab a) a

iterateFix :: Eq a => (a -> a) -> a -> [a]
iterateFix f = apprx . iterate f where
  apprx (x:y:zs) = if x == y then [x] else x: apprx (y:zs)

fix :: (a -> a) -> a
fix f = f (fix f)

fbx n = (0,1,n) $$
         fix (\ f (x,y,k) -> if k == 0 then x else f (y,x+y,k-1))

fbb n = fbbb (0,1,n) where
  fbbb (x,y,n) = if n == 0 then x else fbbb (y,x+y,n-1)

fbc n = fbbc 0 1 n where
  fbbc x y n = if n == 0 then x else fbbc y (x+y) (n-1)

fp' :: Eq a => (a -> a) -> a -> a
fp' f = fix
         (\ g x -> if x == f x then x else g (f x))

until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f = fix
              (\ g x -> if p x then x else g (f x))

while' :: (a -> Bool) -> (a -> a) -> a -> a
while' p f = fix
              (\ g x -> if not (p x) then x else g (f x))

data Color = W | B deriving (Eq,Show)

drawPebble :: [Color] -> [Color]
drawPebble [] = []
drawPebble [x] = [x]
drawPebble (W:W:xs) = drawPebble (B:xs)
drawPebble (B:B:xs) = drawPebble (B:xs)
drawPebble (W:B:xs) = drawPebble (W:xs)
drawPebble (B:W:xs) = drawPebble (W:xs)