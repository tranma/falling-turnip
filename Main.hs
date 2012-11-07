{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

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

import qualified Data.Vector.Unboxed             as V
import qualified Data.Vector.Generic.Mutable     as V
import Control.Monad
import Data.List
import Data.Word
import Data.Bits
import Data.Ix
import System.Random

import World
import Gravity


main :: IO ()                     
main = playArrayIO (InWindow "Falling Turnip" (resX,resY) (200,200))
                   (1,1)
                   60
                   (World { array = R.computeS $ R.fromFunction (Z :. resY :. resX) (const 0)
                          , currentElem = 0
                          , gravityMode = True
                          , mouseDown = False
                          , mousePos  = (0,0) 
                          , mousePrevPos = (0,0) }) 
                   (return     . render)
                   ((return .) . handleInput)
                   stepWorld


handleInput :: Event -> World -> World
handleInput e w = handleInput' (w {mousePrevPos = mousePos w})
  where handleInput' world = case e of
          EventKey (MouseButton LeftButton) Down _ (x,y) -> world { mouseDown = True, mousePos = (x,y) }
          EventKey (MouseButton LeftButton) Up _   (x,y) -> world { mouseDown = False, mousePos = (x,y) }
          EventKey (Char 'w') Down _ _ -> world { currentElem = water }
          EventKey (Char 's') Down _ _ -> world { currentElem = sand }
          EventKey (Char 'b') Down _ _ -> world { currentElem = block }
          EventMotion (x,y) -> world { mousePos = (x,y) }
          _ -> world


-- Updating (each frame) -------------------------------------------------------

resX = 640
resY = 480
resWidth = resX `div` 2
resHeight = resY `div` 2


stepWorld :: Float -> World -> IO World
stepWorld time world
 = do array' <- if mouseDown world 
                then  stepGravity (gravityMode world) 
                  =<< drawLine (mousePrevPos world) (mousePos world) 
                               (currentElem world) (array world)
                else  stepGravity (gravityMode world) 
                   $  R.delay 
                   $  array world
      mode <- (randomIO :: IO Bool) 
      return $ world { array = array', gravityMode = mode } 

-- | Draw a line onto the Repa array
{-# INLINE drawLine #-}
drawLine :: Coord -> Coord -> Cell -> Array U DIM2 Cell -> IO (Array D DIM2 Cell)
drawLine (x0, y0) (x1, y1) new array
  | sh@(Z :. _ :. width) <- R.extent array  
  = let indices 
          = map (\(x,y) -> y * width + x) 
          $ bresenham (round x0 + resWidth, round y0 + resHeight)
                      (round x1 + resWidth, round y1 + resHeight)
    in  do raw  <- V.unsafeThaw $ R.toUnboxed array
           forM_ indices $ \ix -> V.write raw ix new
           raw' <- V.unsafeFreeze raw
           return $ R.delay $ R.fromUnboxed sh raw'


-- | Bresenham's line drawing algorithm, copypasted from Haskell wiki
{-# INLINE bresenham #-}
bresenham :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
bresenham pa@(xa,ya) pb@(xb,yb) = map maySwitch . unfoldr go $ (x1,y1,0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x,y) -> (y,x)) else id
    [(x1,y1),(x2,y2)] = sort [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, error)
        | xTemp > x2 = Nothing
        | otherwise  = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
        where
          tempError = error + deltay
          (newY, newError) = if (2*tempError) >= deltax
                            then (yTemp+ystep,tempError-deltax)
                            else (yTemp,tempError)

