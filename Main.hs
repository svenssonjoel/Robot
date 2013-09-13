

module Main where

import Robot
import Compile
import SimulatorGL

import Control.Monad

---------------------------------------------------------------------------
-- FollowWall
--------------------------------------------------------------------------- 
followWall = 
  while (return true) $ 
    cond checkLeft sMove $ do turnLeft
                              move 
  where
    sMove = do
      cond sensor turnRight move
      
checkLeft :: Program BoolE     
checkLeft = do
  turnLeft
  s <- Sensor
  turnRight 
  return s
  
---------------------------------------------------------------------------
-- World to simulate
--------------------------------------------------------------------------- 
w1 = World North
           (1,8)
           []
           []
           g1

g1 = fromList ((0,0),(9,9))
           (map cell [1,1,1,1,1,1,1,1,1,1,
                      1,0,0,0,0,0,0,0,0,1,
                      1,0,0,0,0,0,0,0,0,1,
                      1,0,0,0,0,0,0,0,0,1,
                      1,0,0,0,0,1,1,1,1,1,
                      1,0,0,0,0,1,0,0,0,1,
                      1,0,0,0,0,0,0,0,0,1,
                      1,0,0,0,0,0,0,0,0,1,
                      1,0,0,0,0,0,0,0,0,1,
                      1,1,1,1,1,1,1,1,1,1])
     where
       cell 0 = Ground
       cell _ = Wall

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------
main = runGL (runCompile followWall) w1
  
