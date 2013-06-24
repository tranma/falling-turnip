{-# LANGUAGE QuasiQuotes #-}

import Data.Array.Repa (Z (..), (:.) (..)) -- , D, DIM2, Array)
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.Vector     as R

import Data.Array.Accelerate.IO
import qualified Data.Array.Accelerate           as A
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
  where frameRate = 30
        pos       = 300
        bareWorld = const nothing

stepWorld :: [(Element, R.Array R.V  R.DIM2 Color)] -> Float -> WorldA -> IO WorldA
stepWorld tooltips time world
 = let curr = mousePos world
       world' = if outOfWorld curr
                then handleUI tooltips curr world
                else world { tooltipRight = blankTooltip }
   in  do int     <- randomRIO (0,100)
          stepped <- if   mouseDown world
                     then do drawn <- drawLineA (mousePrevPos world') curr
                                                (currentElem  world') (array world')
                             return $ step int (currGravityMask world') (A.use $ fromRepa  drawn)
                     else return $ step int (currGravityMask world')
                                 $ A.use $ fromRepa $ array  world'
          array'  <- return $ C.run stepped
          return $ world' { array = toRepa $ array'
                          , currGravityMask = nextGravityMask world'
                          , nextGravityMask = currGravityMask world' }

