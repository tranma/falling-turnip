module World 
       ( Element (..), Cell (..)
       , Env (..)
       , Weight (..), WeightEnv (..)
       , nothing, neon, water, oil, sand, stone, wall
       , isFluid, isWall
       , weight

       , MargPos (..)
       , GlossCoord (..), World (..)
       , resX, resY, resWidth, resHeight, factor
       , render ) 
where

import Data.Array.Repa (D, U, DIM2, Array)
import qualified Data.Array.Repa                 as R
import Graphics.Gloss    

import Data.Word


type Element   = Word32
type Cell      = Word32
type Env       = Word32
type Weight    = Word32
type WeightEnv = Word32

-- | Positions in a Margolus neighbourhood
type MargPos = Int

-- | Coordinates in a Gloss window, origin at center
type GlossCoord = (Float, Float)

data World = World { array        :: Array U DIM2 Cell
                   , currentElem  :: Element
                   , mouseDown    :: Bool 
                   , mousePos     :: GlossCoord 
                   , mousePrevPos :: GlossCoord 
                   , currGravityMask :: Array U DIM2 MargPos
                   , nextGravityMask :: Array U DIM2 MargPos }


-- Gravity tricks require wall to be > 128
{-# INLINE nothing #-}
nothing, neon, water, oil, sand, stone, wall :: Element
nothing = 0
neon    = 1
water   = 5
oil     = 3
sand    = 6
stone   = 7
wall    = 100

{-# INLINE isWall #-}
isWall :: Element -> Bool
isWall x = x == wall

{-# INLINE weight #-}
weight :: Element -> Weight
weight 0 = 1
weight 1 = 0
weight x = fromIntegral x


{-# INLINE isFluid #-}
isFluid :: Element -> Element
--isFluid 0 = 0x40 
isFluid 0 = 0 --0x40
isFluid 3 = 0x40 
isFluid 5 = 0x40 
isFluid _ = 0

{-# INLINE render #-}
render :: World -> Array D DIM2 Color
render world = R.map color $ array world
  where color 1   = light orange        
        color 5   = bright $ bright $ light blue
        color 3   = dark $ dark orange
        color 6   = dim yellow
        color 7   = greyN 0.7
        color 100 = greyN 0.4
        color 0   = black
        color _   = error "render: element doesn't exist"

resX, resY :: Int
resX      = 320
resY      = 240
resWidth  = resX `div` 2
resHeight = resY `div` 2

factor :: Float
factor = 2
