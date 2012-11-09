{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

-- Repa
import Data.Array.Repa (Z (..), (:.) (..), U, DIM2, Array)
import qualified Data.Array.Repa                 as R

-- Gloss
import Graphics.Gloss              
import Graphics.Gloss.Raster.Array 

-- base
import Control.Monad
import Control.Monad.ST
import qualified Data.STRef
import qualified Data.Vector.Unboxed             as UV
import qualified Data.Vector.Generic.Mutable     as MV

-- friends
import World
import Gravity


main :: IO ()                     
main = playArrayIO
  (InWindow "Falling Turnip" (resX,resY) (pos, pos))
  (round factor, round factor)
  frameRate
  (World { array = R.computeS $ R.fromFunction (Z :. resY :. resX) bareWorld
         , currentElem     = nothing
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
        bareWorld = const nothing
        {-sampleWorld (Z:. y :. x)
          | y == 5 = wall
          | (x + y) `mod` 3 == 0 = sand
          | (x - y) `mod` 2 == 0 = water
          | otherwise = nothing-}


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
  = do raw  <- UV.unsafeThaw $ R.toUnboxed array
       stToIO $ bresenham raw (\(x,y)-> y * width + x) new (x0, y0) (x1, y1)
       raw' <- UV.unsafeFreeze raw
       return $ R.fromUnboxed sh raw'
  | otherwise = return array

-- Bresenham's line drawing, copypasted from
-- http://rosettacode.org/wiki/Bitmap/Bresenham's_line_algorithm
-- only destructively updating the array is fast enough
{-# INLINE bresenham #-} 
bresenham vec ix val (xa, ya) (xb, yb)
  = do yV     <- var y1
       errorV <- var $ deltax `div` 2
       forM_ [x1 .. x2] (\x -> do
        y <- get yV
        MV.write vec (if steep then ix (y, x) else ix (x, y)) val        
        mutate errorV $ subtract deltay
        error <- get errorV
        when (error < 0) (do
            mutate yV (+ ystep)
            mutate errorV (+ deltax)))
    where steep = abs (yb - ya) > abs (xb - xa)
          (xa', ya', xb', yb') 
            = if steep 
              then (ya, xa, yb, xb)
              else (xa, ya, xb, yb)
          (x1, y1, x2, y2)
            = if xa' > xb' 
              then (xb', yb', xa', ya')
              else (xa', ya', xb', yb')
          deltax = x2 - x1
          deltay = abs $ y2 - y1
          ystep  = if y1 < y2 then 1 else -1
          var    = Data.STRef.newSTRef
          get    = Data.STRef.readSTRef
          mutate = Data.STRef.modifySTRef
  

