{-# LANGUAGE GADTs,
             KindSignatures,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances, 
             TypeOperators,
             MultiParamTypeClasses,
             OverlappingInstances,
             NoMonomorphismRestriction,
             PolyKinds #-}
module Comp where

import Compile (Prg(..))
import Robot (BoolE(..),true,false)
import Data.Supply
import System.IO.Unsafe

-- Compositional data types

data (e1 :+: e2) x a = InjL (e1 x a) | InjR (e2 x a)
infixr :+:

class sub :<: sup where
  inj :: sub x a  -> sup x a
       
instance f :<: f where
  inj = id

instance (f :<: (f :+: g)) where
   inj = InjL  

instance (f :<: h) => (f :<: (g :+: h)) where
  inj = InjR . inj 
  
inject :: (sub :<: f) => sub (MonadT f) a -> MonadT f a
inject a = In $ Oper $ inj $ a 

-- Monadic operations

data Mops f x a where
  Oper :: f x a -> Mops f x a
  Return :: a -> Mops f x a
  Bind :: x a -> (a -> x b) -> Mops f x b

data MonadT f a = In (Mops f (MonadT f) a)

instance Monad (MonadT f) where 
  return = In . Return
  (>>=)  a f = In (Bind a f) 

-- Compositional robot operations

data MoveOp x a where
  Move :: MoveOp x ()  

data TurnOp x a where
  TurnLeft :: TurnOp x ()
  TurnRight :: TurnOp x ()
  
data CondOp x a where
  Cond :: x BoolE -> x () -> x () -> CondOp x () 

data WhileOp x a where
  While :: x BoolE  -> x () -> WhileOp x ()  

data SensorOp x  a where
  Sensor :: SensorOp x BoolE

move :: (MoveOp :<: f) => MonadT f ()
move =  inject Move 

turnL :: (TurnOp :<: f) => MonadT f ()
turnL = inject TurnLeft

turnR :: (TurnOp :<: f) => MonadT f ()
turnR = inject TurnRight

sensor :: (SensorOp :<: f) => MonadT f BoolE
sensor = inject Sensor 

cond :: (CondOp :<: f) => MonadT f BoolE -> MonadT f () -> MonadT f () -> MonadT f ()
cond b p1 p2 = inject $ Cond b p1 p2

while :: (WhileOp :<: f) => MonadT f BoolE -> MonadT f () -> MonadT f ()
while pb p = inject $ While pb p

-- Compilation

class Compile f where
  compile :: Supply Int -> f a -> (a, Prg)

instance Compile (MoveOp x) where
  compile _ Move = ((), PMove) 

instance Compile (TurnOp x) where
  compile _ TurnLeft = ((), PTurnLeft)
  compile _ TurnRight = ((), PTurnRight)

instance Compile (SensorOp x) where
  compile s Sensor = (Var nom, PSensor nom)
    where
      v = supplyValue s
      nom = "v" ++ show v

instance Compile x => Compile (CondOp x) where
  compile s (Cond b p1 p2) = ((),bp `PSeq` PCond b' p1' p2') 
    where
      (s1,s2,s3) = split3 s
      (b',bp)  = compile s1 b
      (a1,p1') = compile s2 p1
      (a2,p2') = compile s3 p2 

instance Compile x => Compile (WhileOp x) where
  compile s (While wp p) = ((),PWhile nom nwp p')
    where
      (s1,s2,s3) = split3 s
      (b,wp') = compile s1 wp
      (c,p')  = compile s2 p
      nom = "v" ++ (show $ supplyValue s3)
      nwp = wp' `PSeq` PAssign nom b

instance (Compile x, Compile (f x)) => Compile (Mops f x) where
  compile s (Oper o) = compile s o
  compile s (Return a) = (a,PSkip)
  compile s (Bind m f) = (b,prg1 `PSeq` prg2) 
    where
      (s1,s2) = split2 s
      (a,prg1) = compile s1 m
      (b,prg2) = compile s2 (f a)


instance (Compile (f (MonadT f))) => Compile (MonadT f) where
  compile s (In a) = compile s a

instance (Compile (e1 f), Compile (e2 f)) => Compile ((e1 :+: e2) f) where
  compile s (InjL a) = compile s a
  compile s (InjR a) = compile s a 

runCompile prg = snd $ compile s prg
  where s = unsafePerformIO $ newEnumSupply 

-- Test programs

type Robot = MonadT (MoveOp :+: TurnOp :+: CondOp :+: WhileOp :+: SensorOp)

test :: Robot () 
test = do
  move
  move
  cond (return (Lit True)) move move
  while sensor move 

followWall :: Robot () 
followWall = do
  while (return true) $ do
    cond checkLeft sMove (turnL >> move) 
  where
    sMove = do
      cond sensor turnR move  

checkLeft :: Robot BoolE     
checkLeft = do
  turnL
  s <- sensor
  turnR 
  return s

test1 :: Robot () 
test1 = do 
  move
  turnL
  move 
  turnL 
