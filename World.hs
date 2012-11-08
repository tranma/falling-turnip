module World 
       ( Element (..), Cell (..)
       , nothing, water, sand, block
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


block, sand, water, nothing :: Element
nothing = 0
water   = 1
sand    = 2
block   = 255

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
resX = 320
resY = 240
resWidth  = resX `div` 2
resHeight = resY `div` 2

factor :: Float
factor = 2

render :: World -> Array D DIM2 Color
render world = R.map color $ array world
  where color 0   = black
        color 1   = blue
        color 2   = yellow
        color 255 = greyN 0.5
        color _   = error "render: element doesn't exist"
