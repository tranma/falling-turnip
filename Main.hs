{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

import Data.Array.Repa (Z (..), (:.) (..), U, DIM2, Array)
import qualified Data.Array.Repa                 as R

import Graphics.Gloss              
import Graphics.Gloss.Raster.Array 
import Graphics.Gloss.Interface.Pure.Game

import qualified Data.Vector.Unboxed             as V
import qualified Data.Vector.Generic.Mutable     as V
import Control.Monad
import Data.List

import World
import Gravity


main :: IO ()                     
main = playArrayIO
  (InWindow "Falling Turnip" (resX,resY) (pos, pos))
  (round factor, round factor)
  frameRate
  (World { array = R.computeS $ R.fromFunction (Z :. resY :. resX) bareWorld
         , currentElem     = 0
         , currGravityMask = margMaskEven
         , nextGravityMask = margMaskOdd
         , mouseDown       = False
         , mousePos        = (0,0) 
         , mousePrevPos    = (0,0) }) 
  ( return    . render)
  ((return .) . handleInput)
  stepWorld
  where frameRate = 30
        pos       = 300
        bareWorld = const 0
        {-sampleWorld (Z:. y :. x)
          | y == 5 = wall
          | (x + y) `mod` 3 == 0 = sand
          | (x - y) `mod` 2 == 0 = water
          | otherwise = nothing-}


handleInput :: Event -> World -> World
handleInput e w = handleInput' (w {mousePrevPos = mousePos w})
  where handleInput' world = case e of
          EventKey (MouseButton LeftButton) Down _ (x,y) -> world { mouseDown = True, mousePos = (x/factor, y/factor) }
          EventKey (MouseButton LeftButton) Up _   (x,y) -> world { mouseDown = False, mousePos = (x/factor, y/factor) }
          EventKey (Char 'w') Down _ _ -> world { currentElem = water }
          EventKey (Char 's') Down _ _ -> world { currentElem = sand }
          EventKey (Char 't') Down _ _ -> world { currentElem = stone }
          EventKey (Char 'a') Down _ _ -> world { currentElem = wall }
          EventMotion (x,y) -> world { mousePos = (x/factor, y/factor) }
          _ -> world


-- Updating (each frame) -------------------------------------------------------

stepWorld :: Float -> World -> IO World
stepWorld time world
 = do gravitised <- if mouseDown world 
                    then liftM   (stepGravity $ currGravityMask world)
                                $ drawLine (mousePrevPos world) (mousePos world) 
                                           (currentElem  world) (array    world)
                    else return $ stepGravity (currGravityMask world) $ array world
      array' <- R.computeP gravitised
      return $ world { array = array'
                     , currGravityMask = nextGravityMask world
                     , nextGravityMask = currGravityMask world } 


-- | Draw a line onto the Repa array
{-# INLINE drawLine #-}
drawLine :: GlossCoord -> GlossCoord -> Cell -> Array U DIM2 Cell -> IO (Array U DIM2 Cell)
drawLine (xa, ya) (xb, yb) new array
  | sh@(Z :. _ :. width) <- R.extent array  
  , (x0, y0, x1, y1)     <- ( round xa + resWidth, round ya + resHeight
                            , round xb + resWidth, round yb + resHeight )
  , x0 < resX, x1 < resX, y0 < resY, y1 < resY
  = let indices 
          = map (\(x,y) -> y * width + x) 
          $ bresenham (x0,y0) (x1,y1)
    in  do raw  <- V.unsafeThaw $ R.toUnboxed array
           forM_ indices $ \ix -> V.write raw ix new
           raw' <- V.unsafeFreeze raw
           return $ R.fromUnboxed sh raw'
  | otherwise = return array


-- | Bresenham's line drawing algorithm, copypasted from Haskell wiki
--   TODO rewrite to be more fficient
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

