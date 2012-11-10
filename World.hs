module World 
       ( Element (..), Cell (..)
       , Env (..)
       , Weight (..), WeightEnv (..)
       , nothing, steam_water, steam_cndns, fire, fire_end, oil, water, salt_water, sand, salt, stone, torch, plant, spout, wall
       , isFluid, isWall, isFire  
       , weight, age

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


-- Must match on direct values for efficiency
{-# INLINE nothing #-}
nothing, steam_water, steam_cndns, fire, fire_end, oil, water, salt_water, sand, salt, stone, torch, plant, spout, wall :: Element
nothing     = 0
steam_water = 1
steam_cndns = 2
oil         = 6 
water       = 7
salt_water  = 8
sand        = 9
salt        = 10
stone       = 11
fire        = 12
fire_end    = 22
torch       = 23
plant       = 24
spout       = 25
wall        = 100


{-# INLINE isWall #-}
isWall :: Element -> Bool
isWall 100 = True
isWall 25  = True
isWall 24  = True
isWall 23  = True
isWall _   = False

{-# INLINE isFire #-}
isFire :: Element -> Bool
isFire x = x >= fire && x <= fire_end

{-# INLINE isFluid #-}
isFluid :: Element -> Element
isFluid 0 = 0     -- nothing
isFluid 1 = 0x40  -- steam
isFluid 2 = 0x40 
isFluid 6 = 0x40  -- oil
isFluid 7 = 0x40  -- water
isFluid 8 = 0x40  -- salt water
isFluid _ = 0

{-# INLINE weight #-}
weight :: Element -> Weight
weight 0  = 2    -- nothing
weight 1  = 0    -- steam water
weight 2  = 0    -- steam water
weight 10 = 11   -- sand == salt
weight x | isFire x  = 0
         | otherwise = fromIntegral x

{-# INLINE age #-}
age :: Int -> Element -> Element
age gen x 
  | x == fire_end = nothing
  | isFire x      = if gen < 50 then x + 1 else x
  | x == steam_water = if gen < 1 then water else steam_water
  | x == steam_cndns = if gen < 5 then water else steam_cndns
  | otherwise     = x



render :: World -> Array D DIM2 Color
render world = R.map color $ array world
  where color :: Element -> Color
        color 0            = black                                   -- nothing
        color 1            = bright $ light $ light $ light blue     -- steam water           
        color 2            = bright $ light $ light $ light blue     -- steam water           
        color 6            = dark $ dim $ dim orange                 -- oil    
        color 7            = bright $ bright $ light blue            -- water  
        color 8            = bright $ bright $ light $ light blue    -- salt water
        color 9            = dim yellow                              -- sand   
        color 10           = greyN 0.95                              -- salt   
        color 11           = greyN 0.7                               -- stone  
        color 100          = greyN 0.4                               -- wall   
        color 23           = bright $ orange                         -- torch
        color 24           = dim $ green                             -- plant
        color 25           = blue                                    -- spout
        color x | isFire x = mixColors (1.0 * fromIntegral (x - fire)) (1.0 * fromIntegral (fire_end - x)) red yellow 

--        iterate (addColors $ dark $ dim yellow) (dark $ bright red) !! fromIntegral (x - fire)-- fire 
        color _   = error "render: element doesn't exist"


resX, resY :: Int
resX      = 320
resY      = 240
resWidth  = resX `div` 2
resHeight = resY `div` 2

factor :: Float
factor = 1
