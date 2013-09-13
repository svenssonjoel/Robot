
module SimulatorGL where

import Graphics.Rendering.OpenGL hiding (Program,Return,get,Name)
import Graphics.UI.GLUT hiding (Program,Return,get,Name)
import System.Exit

import Data.Array
import Data.Tuple
import Data.Maybe
import Data.IORef

import Control.Monad
import System.IO.Unsafe

import Robot
import Compile

---------------------------------------------------------------------------
-- Robot Orientation
---------------------------------------------------------------------------
data Direction = North | South | East | West
               deriving (Enum,Show)

---------------------------------------------------------------------------
-- Robot World
---------------------------------------------------------------------------
data Cell = Wall | Ground 
          deriving (Eq, Show)

type Grid = Array (Int,Int) Cell

size :: Grid -> (Int,Int) 
size = swap . snd . bounds 

(*!) :: Grid -> (Int,Int) -> Cell
(*!) g (x,y) = (!) g (y,x) 

fromList :: ((Int,Int),(Int,Int)) -> [Cell] -> Grid 
fromList = listArray

type Pos       = (Int,Int)

type Line      = (Pos,Pos)  -- center of cell to center of cell


data World     = World { robotDir   :: Direction, 
                         robotPos   :: Pos,
                         robotEnv   :: Env,
                         robotTrack :: [Line],
                         worldGrid  :: Grid}
                 deriving Show 

tPos :: Direction -> Pos -> Pos    
tPos North (x,y) = (x,y-1)
tPos South (x,y) = (x,y+1)
tPos East  (x,y) = (x+1,y)
tPos West  (x,y) = (x-1,y)

turnR :: Direction -> Direction 
turnR North = East
turnR East = South
turnR South = West
turnR West = North

turnL = turnR . turnR . turnR
---------------------------------------------------------------------------
--Color Scheme
---------------------------------------------------------------------------
groundColor = Color4 (0.6 :: GLclampf) 0.6 0.6 0.0
robotColor  = Color4 (1.0 :: GLclampf) 1.0 1.0 0.0
lineColor   = Color4 (0.0 :: GLclampf) 0.0 0.0 0.0
wallColor   = Color4 (1.0 :: GLclampf) 1.0 1.0 0.0

---------------------------------------------------------------------------
--runGL
---------------------------------------------------------------------------
runGL :: Prg -> World -> IO ()
runGL prg w = do 
  (_, _) <- getArgsAndInitialize
  createWindow "RobotGL"
  windowSize $= Size (fromIntegral sx) (fromIntegral sy) 
  viewport   $= (Position 0 0, Size (fromIntegral sx) (fromIntegral sy))
  scale ((1/(fromIntegral sx/2)) :: GLdouble) (-1/(fromIntegral sy/2)) 0
  translate $ Vector3 ((-(fromIntegral sx/2)) :: GLdouble) (-(fromIntegral sy/2)) 0
  
  ioref <- newIORef (prg,w) 

  clearColor $= groundColor

  lineSmooth $= Enabled
  lineWidth $= 4
  lineStipple $= Just (1,0xf0f0) 
  
  addTimerCallback 100 (timer ioref)
  displayCallback $= (display ioref) 
  mainLoop
  where
    (wx,wy) = size (worldGrid w) 
    (sx,sy) = (64*(wx+1),64*(wy+1)) 
  
timer :: IORef (Prg, World) -> IO ()  
timer ioref = do
  (prg,world) <- readIORef ioref

  let (prg',world') = stepProgram prg world
      
  putStrLn $ show prg ++ "\n"

  writeIORef ioref (prg',world')

  
  if (prg' == PSkip)
    then putStrLn "Done!" 
    else addTimerCallback 100 (timer ioref)
  postRedisplay Nothing
  flush

display :: IORef (Prg, World) -> IO ()
display ioref = do
  

  clear [ColorBuffer]


  (_,world) <- readIORef ioref
  let (wx,wy) = size (worldGrid world)
  let sinfo = (fromIntegral ((wx+1)*64), fromIntegral ((wy+1)*64))


  renderGrid (worldGrid world) 

  renderLines (robotTrack world)

  putStrLn $ show $ robotDir world
  
  renderRobot (robotPos world) (robotDir world) sinfo
  
  flush


---------------------------------------------------------------------------
-- Step through program
--------------------------------------------------------------------------- 
stepProgram (PSeq PSkip p1) w = stepProgram  p1 w -- Eat skips
stepProgram (PSeq p1 p2) w   = (PSeq prg' p2, w') 
  where
    (prg',w') = stepProgram p1 w
stepProgram (PSensor nom) w = (PSkip,w')
  where
    cell = worldGrid w *! (tPos (robotDir w) (robotPos w))
    env' = (nom,cell == Wall) : robotEnv w
    w'   = w {robotEnv = env'}
stepProgram PMove w = (PSkip,w {robotPos = np,
                                robotTrack = l})
  where
    np  = tPos (robotDir w) (robotPos w)
    lines = robotTrack w
    l   = (robotPos w,np): lines 
stepProgram PTurnLeft w = (PSkip, w {robotDir = turnL (robotDir w)})
stepProgram PTurnRight w = (PSkip, w {robotDir = turnR  (robotDir w)})

stepProgram (PCond b p1 p2) w = if fromJust b'  -- TODO: fromJust ! 
                                then stepProgram p1 w
                                else stepProgram p2 w 
  where
    b' = evalExp (robotEnv w) b

stepProgram prg@(PWhile var pb p1) w = stepProgram (PCond (Var var) (p1 `PSeq` prg) PSkip) w'
  where
      w' = runProgram pb w
      --b  = fromJust $ lookup var (robotEnv w')
      --w'' = runProgram p1 w'
    
-- Cond pb (p1 >> PWhile) Skip     
--stepProgram (PWhile var pb p1) w = (if b then PWhile var pb p1 else PSkip,
--                                    if b then w'' else w') 
--  
--  where
--    w' = runProgram pb w
--    b  = fromJust $ lookup var (robotEnv w')
--    w'' = runProgram p1 w'
    

stepProgram (PAssign nom e) w = (PSkip,w')
  where 
    env = robotEnv w
    -- TODO: More fromJust to remove ! 
    w'  = w {robotEnv = (nom,fromJust (evalExp env e)):env}
stepProgram PSkip w = (PSkip,w)



runProgram :: Prg -> World -> World 
runProgram p w = snd $ until ((==PSkip) . fst) (uncurry stepProgram) (p,w) 


---------------------------------------------------------------------------
-- Rendering 
---------------------------------------------------------------------------
renderGrid :: Grid -> IO ()
renderGrid grid =
  do 
    zipWithM_ renderCell stens offsets
  where coords  = concat [[(x,y) | x <- [0..wx+1]]| y <- [0..wy+1]]
        offsets = map m64 coords
        m64 (x,y) = (x*64,y*64)
        stens = map (stencil grid) coords
        (wx,wy) = size grid
        
stencil :: Grid -> (Int,Int) -> [Cell]
stencil grid (ix,iy)  = map safeIx indices 
  where
    indices = [(ix,iy-1),(ix,iy+1),(ix,iy),(ix-1,iy),(ix+1,iy)]
    safeIx (ix,iy) = if (ix < 0 || ix > bx || iy < 0 || iy > by)
                     then Ground
                     else grid *! (ix,iy)
    (bx,by) = size grid 

-- Creates a Cell 
renderCell :: [Cell] -> (Int,Int) -> IO ()
renderCell [n,s,Wall,e,w] p =
  do
    color wallColor
    render 0 n p 
    render 1 s p 
    render 2 e p 
    render 3 w p 
    render 4 Wall p
renderCell [_,_,Ground,_,_] _ = return () 
renderCell _ _ = error "renderCell: incorrect stencil" 
                           
                           
render :: Int -> Cell -> (Int,Int) -> IO () 
render kind Wall p = do
    renderPrimitive Quads $
      mapM_ vert (map (trans p) (shapes !! kind))
render _ Ground _ = return () 

trans (tx,ty) (x,y) = (tx+x,ty+y)

vert :: (Int,Int) -> IO ()
vert (x,y) = vertex$ Vertex3 (fromIntegral x) (fromIntegral y) (0.0 :: GLdouble)

shapes = [[(23,0),(39,0),(39,23),(23,23)],   -- Top  0
          [(23,39),(39,39),(39,64),(23,64)], -- Bot  1
          [(0,23),(23,23),(23,39),(0,39)],   -- Left 2 
          [(39,23),(64,23),(64,39),(39,39)], -- right 3
          [(23,23),(39,23),(39,39),(23,39)]] -- Center piece 4

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
renderRobot :: Pos -> Direction -> (GLdouble,GLdouble) -> IO ()
renderRobot (x,y) d (wx,wy) =
  do
    preservingMatrix $
      do
        loadIdentity
        scale ((1/(wx/2)) :: GLdouble) (-1/(wy/2)) 0
        

        translate $ Vector3 ((fromIntegral x*64+32) :: GLdouble) (fromIntegral y*64+32) 0
        translate $ Vector3 ((-(wx/2)) :: GLdouble) (-(wy/2)) 0

        rotate (dir d) $ Vector3 0 0 (1 :: GLdouble)
        
        color robotColor
        renderPrimitive Triangles $
          mapM_ vert robotshape
  where
    robotshape = [(-10,10),(0,-10),(10,10)]
    dir North = 0
    dir South = 180
    dir East  = 90
    dir West  = 270

renderLines :: [((Int,Int),(Int,Int))] -> IO ()
renderLines lines  = do
  color lineColor 
  mapM_ renderLine (map transLine lines)
  where
    renderLine ((x1,y1),(x2,y2)) = do
     renderPrimitive Lines $ do
          vertex $ Vertex3 (x1 :: GLdouble) y1 0
          vertex $ Vertex3 (x2 :: GLdouble) y2 0 
    transLine ((x1,y1),(x2,y2)) = ((fromIntegral x1*64+32,fromIntegral y1*64+32), 
                                   (fromIntegral x2*64+32,fromIntegral y2*64+32))
  
