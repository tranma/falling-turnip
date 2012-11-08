module World 
       ( Element (..), Cell (..)
       , nothing, water, sand, stone, wall
       , MargPos (..)
       , GlossCoord (..), World (..)
       , resX, resY, resWidth, resHeight, factor
       , render ) 
where

import Data.Array.Repa (D, U, DIM2, Array)
import qualified Data.Array.Repa                 as R
import Graphics.Gloss              

import Data.Word

type Element = Word32
type Cell    = Element

-- Temporary tricks for fast gravity and fluid motion
--  wall must be > 128
wall, stone, sand, water, nothing :: Element
nothing = 0
water   = 2
sand    = 3
stone   = 5
wall    = 255

type MargPos  = Int

type GlossCoord = (Float, Float)
data World = World { array        :: Array U DIM2 Cell
                   , currentElem  :: Element
                   , mouseDown    :: Bool 
                   , mousePos     :: GlossCoord 
                   , mousePrevPos :: GlossCoord 
                   , currGravityMask :: Array U DIM2 MargPos
                   , nextGravityMask :: Array U DIM2 MargPos }

resX, resY :: Int
resX = 640
resY = 480
resWidth  = resX `div` 2
resHeight = resY `div` 2

factor :: Float
factor = 2

render :: World -> Array D DIM2 Color
render world = R.map color $ array world
  where color 0   = black
        color 2   = light blue
        color 3   = yellow
        color 5   = greyN 0.3
        color 255 = greyN 0.7
        color _   = error "render: element doesn't exist"
