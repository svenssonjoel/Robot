{-# LANGUAGE GADTs #-} 
module Robot where

import Control.Monad
import Data.Supply

import System.IO.Unsafe

---------------------------------------------------------------------------
-- Boolean Expressions 
---------------------------------------------------------------------------
type Name = String

data BoolE = Lit Bool
           | Var Name
           | (:||:) BoolE BoolE
           | (:&&:)  BoolE BoolE
           | Not BoolE  
         deriving Eq


true, false :: BoolE 
true = Lit True
false = Lit False

printExp :: BoolE -> String
printExp (Lit b) = show b
printExp (Var nom) = nom
printExp (a :||: b) = "(" ++ printExp a ++ " || " ++ printExp b ++ ")"
printExp (a :&&: b) = "(" ++ printExp a ++ " && " ++ printExp b ++ ")"
printExp (Not a)    = "(" ++ "not" ++ " " ++ printExp a ++ ")"

instance Show BoolE where
  show = printExp 

---------------------------------------------------------------------------
-- Evaluate an expression in an environment
--------------------------------------------------------------------------- 
type Env = [(String, Bool)]

evalExp :: Env -> BoolE -> Maybe Bool
evalExp e (Lit a) = Just a
evalExp e (Var str) = lookup str e
evalExp e (e1 :||: e2) = liftM2 (||) (evalExp e e1) (evalExp e e2)
evalExp e (e1 :&&: e2) = liftM2 (&&) (evalExp e e1) (evalExp e e2)
evalExp e (Not e1)      = liftM  not (evalExp e e1)

---------------------------------------------------------------------------
-- Program datatype
---------------------------------------------------------------------------

data Program a where
  Move      :: Program ()
  TurnRight :: Program ()
  TurnLeft  :: Program ()

  Sensor    :: Program BoolE
  Cond      :: Program BoolE -> Program () -> Program () -> Program () 

  While     :: Program BoolE -> Program () -> Program ()

-- Monad
  Return    :: a -> Program a 
  Bind      :: Program a -> (a -> Program b) -> Program b


instance Monad Program where
  return = Return
  (>>=)  = Bind

instance Functor Program where 
  fmap = liftM

instance Applicative Program where 
  pure = return 
  (<*>) u v = 
    do 
     f <- u 
     x <- v
     return (f x) 

move = Move
turnRight = TurnRight
turnLeft = TurnLeft
sensor = Sensor
cond = Cond
while = While

---------------------------------------------------------------------------
-- Print a program
---------------------------------------------------------------------------

mkName :: Supply Int -> String
mkName s = "v" ++ show (supplyValue s)


printPrg :: Program a -> String
printPrg p = snd $ printPrg' s p
  where
    s = unsafePerformIO $ newEnumSupply

printPrg' :: Supply Int -> Program a -> (a, String) 
printPrg' s Move = ((),"move\n")
printPrg' s TurnRight = ((),"turnR\n")
printPrg' s TurnLeft = ((),"turnL\n")
printPrg' s Sensor = (Var nom,nom ++ " <- sensor\n")
  where
    nom = mkName s
printPrg' s (Cond b p1 p2) = ((),bstr ++ str) -- (varE nom,str)
  where str = "if " ++ printExp b' ++ "\n" ++
              "then\n" ++ s1 ++ 
              "else\n" ++ s2
        (b',bstr) = printPrg' c4 b  
        (a,s1) = printPrg' c1 p1
        (c,s2) = printPrg' c2 p2
        (c1,c2,c3,c4) = split4 s
        nom    = mkName c3
printPrg' s (While pb p) = ((),str)
  where
    (s1,s2) = split2 s 
    str = "while {" ++ snd (printPrg' s1 pb) ++ "}\n" ++
          "do\n" ++ 
          snd (printPrg' s1 p) ++ 
          "done\n"
          
printPrg' s (Return a) = (a, "")
printPrg' s (Bind pa f) = (b,str ++ str2) 
  where
    (s1,s2) = split2 s 
    (a,str) = printPrg' s1 pa 
    prg2 = f a
    (b,str2) = printPrg' s2 prg2
