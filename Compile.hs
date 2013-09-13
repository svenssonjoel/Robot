{-# LANGUAGE GADTs #-} 

module Compile where

import Control.Monad
import Data.Supply
import System.IO.Unsafe

import Robot


---------------------------------------------------------------------------
-- Target language
---------------------------------------------------------------------------

data Prg = PMove          
         | PTurnRight
         | PTurnLeft
         | PSensor Name    -- knows a unique name
         | PCond BoolE Prg Prg
         | PWhile Name Prg Prg -- the Var points out where the result of Prg is stored
           
         -- PSeq replaces Bind   
         | PSeq Prg Prg
         -- PSkip replaces Return   
         | PSkip            
         -- PAssign is used by While in Program.   
         | PAssign Name BoolE
           deriving (Eq, Show)



---------------------------------------------------------------------------
-- The compiler
---------------------------------------------------------------------------

runCompile :: Program a -> Prg
runCompile prg = snd $ compile s prg 
  where 
    s = unsafePerformIO $ newEnumSupply
    
compile :: Supply Int -> Program a -> (a, Prg) 
compile s Move = ((),PMove)
compile s TurnRight = ((),PTurnRight)
compile s TurnLeft  = ((),PTurnLeft)
compile s Sensor    = (Var nom,PSensor nom)
  where
    v = supplyValue s
    nom = "v" ++ show v 
compile s (Cond b p1 p2) = ((),bp `PSeq` PCond b' p1' p2') 
  where
    (s1,s2,s3) = split3 s
    (b',bp)  = compile s1 b
    (a1,p1') = compile s2 p1
    (a2,p2') = compile s3 p2 
compile s (While wp prg) = ((),PWhile nom nwp prg') 
  where
    (s1,s2,s3) = split3 s
    (b,wp') = compile s1 wp
    (c,prg') = compile s2 prg 
    nom  = "v" ++ (show $ supplyValue s3)
    nwp = (wp' `PSeq` PAssign nom b) 
    
compile s (Return a) = (a,PSkip)

compile s (Bind pa f) = (b, prg1 `PSeq` prg2) 
  where
    (s1,s2) = split2 s
    (a,prg1) = compile s1 pa
    (b,prg2) = compile s2 (f a) 
