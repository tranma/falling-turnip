module World 
       ( Element (..), Cell (..)
       , nothing, water, sand, block
       , Coord (..), World (..)
       , render ) 
where

import Data.Array.Repa (D, U, DIM2, Array)
import qualified Data.Array.Repa                 as R
import Graphics.Gloss              
import Data.Word


type Element = Word64
type Cell    = Word64

block, sand, water, nothing :: Element
nothing = 0
water   = 1
sand    = 2
block   = 100

type Coord = (Float, Float)
data World = World { array        :: Array U DIM2 Cell
                   , currentElem  :: Element
                   , mouseDown    :: Bool 
                   , mousePos     :: Coord 
                   , mousePrevPos :: Coord 
                   , gravityMode  :: Bool }

render :: World -> Array D DIM2 Color
render world = R.map color $ array world
  where color 0   = black
        color 1   = blue
        color 2   = yellow
        color 100 = greyN 0.5
        color _   = red
