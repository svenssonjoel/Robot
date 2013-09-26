{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE IncoherentInstances   #-}
module Comp where

import Robot (BoolE(..),true,false)
import Compile (Prg(..))
import Data.Supply
import System.IO.Unsafe

data (e1 :+: e2) (x :: * -> *) a
  = InjL (e1 x a) | InjR (e2 x a)

infixr :+:

class (sub :: (* -> *) -> * -> *) :<: sup where
  inj :: sub x a -> sup x a

instance f :<: f where
  inj = id

instance (f :<: (f :+: g)) where
  inj = InjL

instance (f :<: h) => (f :<: (g :+: h)) where
  inj = InjR . inj

data MoveOp (x :: * -> *) a where
  Move :: MoveOp x ()

data TurnOp (x :: * -> *) a where
  TurnLeft  :: TurnOp x ()
  TurnRight :: TurnOp x ()

data SensorOp (x :: * -> *) a where
  Sensor :: SensorOp x BoolE

data CondOp x a where
  Cond :: x BoolE -> x () -> x () -> CondOp x ()

data WhileOp x a where
  While :: x BoolE -> x () -> WhileOp x ()

runCompile :: Compile f => f a -> Prg
runCompile prg = snd $ compile s prg
  where s = unsafePerformIO $ newEnumSupply

type NameSupply = Supply Int

class Compile f where
  compile :: NameSupply -> f a -> (a, Prg)

instance Compile (MoveOp x) where
  compile _ Move = ((),PMove)

instance Compile (TurnOp x) where
  compile _ TurnLeft  = ((), PTurnLeft)
  compile _ TurnRight = ((), PTurnRight)

instance Compile (SensorOp x) where
  compile s Sensor = (Var nom, PSensor nom)
    where v   = supplyValue s
          nom = "v" ++ show v

instance Compile x => Compile (CondOp x) where
  compile s (Cond b p1 p2) =
    ((), bp `PSeq` PCond b' p1' p2')
    where (s1,s2,s3) = split3 s
          (b',bp)    = compile s1 b
          (a1,p1')   = compile s2 p1
          (a2,p2')   = compile s3 p2

instance Compile x => Compile (WhileOp x) where
  compile s (While wp p) = ((),PWhile nom nwp p')
    where (s1,s2,s3) = split3 s
          (b,wp')    = compile s1 wp
          (c,p')     = compile s2 p
          nom        = "v" ++ (show $ supplyValue s3)
          nwp        = wp' `PSeq` PAssign nom b

instance (Compile (e1 f), Compile (e2 f))
  => Compile ((e1 :+: e2) f) where
  compile s (InjL a) = compile s a
  compile s (InjR a) = compile s a

data Mops f x a where
  Oper   :: f x a -> Mops f x a
  Return :: a -> Mops f x a
  Bind   :: x a -> (a -> x b) -> Mops f x b

data MonadExp f a = In (Mops f (MonadExp f) a)

instance Monad (MonadExp f) where
  return    = In . Return
  (>>=) a f = In (Bind a f)

type Robot = MonadExp (MoveOp :+: TurnOp  :+:
                       CondOp :+: WhileOp :+:
                       SensorOp)

inject :: (sub :<: f)
       => sub (MonadExp f) a
       -> MonadExp f a
inject a = In $ Oper $ inj $ a

move :: (MoveOp :<: f) => MonadExp f ()
move = inject Move

turnL :: (TurnOp :<: f) => MonadExp f ()
turnL = inject TurnLeft

turnR :: (TurnOp :<: f) => MonadExp f ()
turnR = inject TurnRight

sensor :: (SensorOp :<: f) => MonadExp f BoolE
sensor = inject Sensor

cond :: (CondOp :<: f)
     => MonadExp f BoolE
     -> MonadExp f () -> MonadExp f () -> MonadExp f ()
cond b p1 p2 = inject $ Cond b p1 p2

while :: (WhileOp :<: f)
      => MonadExp f BoolE
      -> MonadExp f () -> MonadExp f ()
while pb p = inject $ While pb p

instance (Compile x, Compile (f x))
  => Compile (Mops f x) where
  compile s (Oper o)   = compile s o
  compile s (Return a) = (a,PSkip)
  compile s (Bind m f) = (b,prg1 `PSeq` prg2)
    where (s1,s2)  = split2 s
          (a,prg1) = compile s1 m
          (b,prg2) = compile s2 (f a)

instance (Compile (f (MonadExp f)))
  => Compile (MonadExp f) where
  compile s (In a) = compile s a

test1 :: Robot ()
test1 = do
  move
  turnL
  move
  turnL
