{-# LANGUAGE PatternGuards #-}
module Common.Draw
       (drawLineU, drawLineA)
where

-- Repa
import Data.Array.Repa (Z (..), (:.) (..), U, DIM2, Array)
import qualified Data.Array.Repa                 as R

-- Acc
import Data.Array.Accelerate.IO

-- base
import Control.Monad
import Control.Monad.ST
import qualified Data.STRef
import qualified Data.Vector.Unboxed             as UV
import qualified Data.Vector.Generic.Mutable     as MV

import Common.World


-- | Draw a line onto the Repa array
drawLineU :: GlossCoord -> GlossCoord -> Cell -> Array U DIM2 Cell -> IO (Array U DIM2 Cell)
drawLineU (xa, ya) (xb, yb) new array
  | sh@(Z :. _ :. width) <- R.extent array
  , (x0, y0, x1, y1)     <- ( round xa + resWidth, round ya + resHeight
                            , round xb + resWidth, round yb + resHeight )
  , x0 < resX - 2, x1 < resX - 2, y0 < resY - 2, y1 < resY - 2, x0 > 2, y0 > 2, x1 > 2, y1 > 2
  = do raw  <- UV.unsafeThaw $ R.toUnboxed array
       stToIO $ bresenham raw (\(x,y)-> y * width + x) new (x0, y0) (x1, y1)
       raw' <- UV.unsafeFreeze raw
       return $ R.fromUnboxed sh raw'
  | otherwise = return array

-- | Draw a line onto the Repa array backed by Accelerate
drawLineA :: GlossCoord -> GlossCoord -> Cell -> Array A DIM2 Cell -> IO (Array A DIM2 Cell)
drawLineA (xa, ya) (xb, yb) new array
  -- FIXME input check here as well, maybe faster
  = R.copyP array >>= drawLineU (xa, ya) (xb, yb) new >>= R.copyP

-- Bresenham's line drawing, copypasted from
-- http://rosettacode.org/wiki/Bitmap/Bresenham's_line_algorithm
-- only destructively updating the array is fast enough
bresenham vec ix val (xa, ya) (xb, yb)
  = do yV     <- var y1
       errorV <- var $ deltax `div` 2
       forM_ [x1 .. x2] (\x -> do
        y <- get yV
        drawCirc $ if steep then (y, x) else (x, y)
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
          drawCirc (x,y) = do MV.write vec (ix (x,y)) val     -- me
                              MV.write vec (ix (x,y+1)) val   -- top
                              MV.write vec (ix (x+1,y+1)) val -- top right
                              MV.write vec (ix (x+1,y)) val   -- right
                              MV.write vec (ix (x+1,y-1)) val -- down right
                              MV.write vec (ix (x,y-1)) val   -- down
                              MV.write vec (ix (x-1,y-1)) val -- down left
                              MV.write vec (ix (x-1,y)) val   -- left
                              MV.write vec (ix (x-1,y+1)) val -- top left
                              MV.write vec (ix (x,y+2)) val   -- top top
                              MV.write vec (ix (x+2,y)) val   -- right right
                              MV.write vec (ix (x-2,y)) val   -- left left
                              MV.write vec (ix (x,y-2)) val   -- down down
