{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TupleSections, FlexibleContexts #-}

-- Repa
import Data.Array.Repa (Z (..), (:.) (..))
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.Vector     as R

-- Gloss
import Graphics.Gloss
import Graphics.Gloss.Raster.Array

-- base
import Control.Monad
import System.Random

-- friends
import Repa.Step

import Common.Draw
import Common.World
import Common.Event

main :: IO ()
main = do
  tooltips <- mapM loadTooltip tooltipFiles
  playArrayIO
    (InWindow "Falling Turnip" (winX * round factor, winY * round factor) (pos, pos))
    (round factor, round factor)
    frameRate
    (World { array = (R.computeS $ R.fromFunction (Z :. resY :. resX) bareWorld) :: R.Array R.U R.DIM2 Cell
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

stepWorld :: [(Element, R.Array R.V  R.DIM2 Color)] -> Float -> WorldR -> IO WorldR
stepWorld tooltips time world
 = let curr = mousePos world
       world' = if outOfWorld curr then handleUI tooltips curr world else world { tooltipRight = blankTooltip }
   in  do int     <- randomRIO (0,100)
          stepped <- if   mouseDown world
                     then liftM (step int $ currGravityMask world')
                             $  drawLineU (mousePrevPos world') curr
                                         (currentElem  world') (array world')
                     else return $ step int (currGravityMask world')
                                 $ array world'
          array'  <- R.computeP stepped
          return $ world' { array = array'
                          , currGravityMask = nextGravityMask world'
                          , nextGravityMask = currGravityMask world' }

