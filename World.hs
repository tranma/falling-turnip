module World 
       ( Element (..), Cell (..)
       , Env (..)
       , Weight (..), WeightEnv (..)
       , nothing, steam_water, fire, oil, water, salt_water, sand, salt, stone, wall
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


--    match on direct values for efficiency!!!
{-# INLINE nothing #-}
nothing, steam_water, fire, oil, water, salt_water, sand, salt, stone, wall :: Element
nothing     = 0
steam_water = 1
fire        = 2
oil         = 3
water       = 5
salt_water  = 6
sand        = 7
salt        = 8
stone       = 9
wall        = 100

{-# INLINE isWall #-}
isWall :: Element -> Bool
isWall x = x == wall

{-# INLINE weight #-}
weight :: Element -> Weight
weight 0 = 2  -- nothing
weight 1 = 0  -- steam water
weight 2 = 0  -- fire
weight 7 = 8  -- sand == salt
weight x = fromIntegral x

{-# INLINE isFluid #-}
isFluid :: Element -> Element
isFluid 0 = 0     -- nothing
isFluid 3 = 0x40  -- oil
isFluid 5 = 0x40  -- water
isFluid 6 = 0x40  -- salt water
isFluid _ = 0

render :: World -> Array D DIM2 Color
render world = R.map color $ array world
  where color 0   = black                                   -- nothing
        color 1   = bright $ light $ light $ light blue     -- steam water   
        color 2   = bright red                              -- fire   
        color 3   = dark $ dim $ dim orange                 -- oil    
        color 5   = bright $ bright $ light blue            -- water  
        color 6   = bright $ bright $ light $ light blue    -- salt water
        color 7   = dim yellow                              -- sand   
        color 8   = greyN 0.95                              -- salt   
        color 9   = greyN 0.7                               -- stone  
        color 100 = greyN 0.4                               -- wall   
        color _   = error "render: element doesn't exist"

resX, resY :: Int
resX      = 320
resY      = 240
resWidth  = resX `div` 2
resHeight = resY `div` 2

factor :: Float
factor = 2
