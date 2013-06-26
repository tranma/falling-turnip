{-# LANGUAGE QuasiQuotes #-}

import Data.Array.Repa (Z (..), (:.) (..)) -- , D, DIM2, Array)
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.Vector     as R

import Data.Array.Accelerate.IO
import qualified Data.Array.Accelerate.CUDA      as C

import Graphics.Gloss
import Graphics.Gloss.Raster.Array

import System.Random

import Common.Draw
import Accelerate.World
import Common.Event
import Accelerate.Step

main :: IO ()
main = do
  tooltips <- mapM loadTooltip tooltipFiles
  playArrayIO
    (InWindow "Falling Turnip" (winX * round factor, winY * round factor) (pos, pos))
    (round factor, round factor)
    frameRate
    (World { array = computeAccS $ R.fromFunction (Z :. resY :. resX) bareWorld
           , currentElem     = nothing
           , currGravityMask = margMaskEven
           , nextGravityMask = margMaskOdd
           , mouseDown       = False
           , mousePos        = (0,0)
           , mousePrevPos    = (0,0)
           , tooltipLeft     = blankTooltip
           , tooltipRight    = blankTooltip
           })
    ( return    . render)
    ((return .) . handleInput)
    (stepWorld tooltips)
  where frameRate = 10
        pos       = 300
        bareWorld = const nothing

stepWorld :: [(Element, R.Array R.V  R.DIM2 Color)] -> Float -> WorldA -> IO WorldA
stepWorld tooltips time world
 = let curr = mousePos world
       world' = if outOfWorld curr
                then handleUI tooltips curr world
                else world { tooltipRight = blankTooltip }
       step' int acc = step int (currGravityMask world') acc
   in  do int     <- randomRIO (0,100)
          drawn   <- if   mouseDown world
                     then drawLineA (mousePrevPos world') curr
                                    (currentElem  world') (array world')
                     else return $ array world'
          return $ world' { array = toRepa $ C.run1 (step' int) (fromRepa drawn)
                          , currGravityMask = nextGravityMask world'
                          , nextGravityMask = currGravityMask world' }

