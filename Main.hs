{-# LANGUAGE QuasiQuotes #-}

import Data.Array.Repa (Z (..), (:.) (..), D, U, DIM2, Array, (!))
import Data.Array.Repa.Repr.Cursored (C)
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2 
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Eval            as R
import qualified Data.Array.Repa.Repr.Cursored   as R
import qualified Data.Array.Repa.Stencil         as R
import qualified Data.Array.Repa.Stencil.Dim2    as R

import Graphics.Gloss              
import Graphics.Gloss.Raster.Array 
import Graphics.Gloss.Interface.Pure.Game

type Life = Int
type Coord = (Float, Float)
data World = World { arr :: Array U DIM2 Life
                   , mouseDown :: Bool 
                   , mousePos  :: Coord }

conwayStencil :: Stencil DIM2 Life
conwayStencil = [stencil2| 1 1 1
                           1 0 1
                           1 1 1 |]

delta :: Life -> Int -> Life
delta 1 n | n < 2     = 0
          | n > 3     = 0
          | otherwise = 1
delta 0 n | n == 3    = 1
          | otherwise = 0

tick :: Array D DIM2 Life -> Array U DIM2 Life
tick a = R.suspendedComputeP $ R.szipWith delta a $ R.mapStencil2 (BoundConst 0) conwayStencil a

resX = 640
resY = 480

step :: Float -> World -> World
step time world
 = let updateMouse = if mouseDown world 
                     then updatePoint (mousePos world) 1 
                     else R.delay
   in  world { arr = tick $ updateMouse (arr world) } 

updatePoint :: Coord -> Life -> Array U DIM2 Life -> Array D DIM2 Life
updatePoint (x,y) newLife arr
 = let same a a' n = abs (round a + (n `div` 2) - a') < 5
   in  R.fromFunction (Z :. resY :. resX) 
                      (\sh@(Z :. y' :. x') 
                        -> if same x x' resX && same y y' resY 
                           then newLife 
                           else arr ! sh)  

handleInput :: Event -> World -> World
handleInput e world = case e of
  EventKey (MouseButton LeftButton) Down _ (x,y) -> world { mouseDown = True, mousePos = (x,y) }
  EventKey (MouseButton LeftButton) Up _   (x,y) -> world { mouseDown = False, mousePos = (x,y) }
  EventMotion (x,y) -> world { mousePos = (x,y) }

render :: World -> Array D DIM2 Color
render world = R.map color $ arr world
  where color 0 = black
        color _ = white

main :: IO ()                     
main = playArray (InWindow "Conway" (resX,resY) (50,50))
                 (1,1)
                 10
                 (World { arr = R.computeS $ R.fromFunction (Z :. resY :. resX) foo
                        , mouseDown = False
                        , mousePos  = (0,0) }) 
                 render
                 handleInput
                 step

foo sh@(Z :. y :. x) = if (x `div` (y + 1)) == 0 then 0 else 1
