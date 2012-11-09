module World 
       ( Element (..), Cell (..)
       , nothing, water, oil, sand, stone, wall
       , isFluid, weight, isWall
       , MargPos (..), Weight (..)
       , GlossCoord (..), World (..)
       , resX, resY, resWidth, resHeight, factor
       , render, handleInput ) 
where

import Data.Array.Repa (D, U, DIM2, Array)
import qualified Data.Array.Repa                 as R
import Graphics.Gloss    
import Graphics.Gloss.Interface.Pure.Game          

import Data.Word


-- Basic world constructs ------------------------------------------------------
type Weight  = Word32
type Element = Word32
type Cell    = Element

-- Gravity tricks require wall to be > 128
{-# INLINE nothing #-}
nothing, water, oil, sand, stone, wall :: Element
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
        color 5   = bright $ light blue
        color 3   = dark (dark orange)
        color 6   = dim yellow
        color 7   = greyN 0.7
        color 100 = greyN 0.4
        color 0   = black
        color _   = error "render: element doesn't exist"


-- Drawing ---------------------------------------------------------------------

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

handleInput :: Event -> World -> World
handleInput e w = handleInput' (w {mousePrevPos = mousePos w})
  where handleInput' world = case e of
          EventKey (MouseButton LeftButton) Down _ (x,y) -> world { mouseDown = True, mousePos = (x/factor, y/factor) }
          EventKey (MouseButton LeftButton) Up _   (x,y) -> world { mouseDown = False, mousePos = (x/factor, y/factor) }
          EventKey (Char 'n') Down _ _ -> world { currentElem = neon  }
          EventKey (Char 'w') Down _ _ -> world { currentElem = water }
          EventKey (Char 'o') Down _ _ -> world { currentElem = oil   }
          EventKey (Char 's') Down _ _ -> world { currentElem = sand  }
          EventKey (Char 't') Down _ _ -> world { currentElem = stone }
          EventKey (Char 'a') Down _ _ -> world { currentElem = wall  }
          EventMotion (x,y) -> world { mousePos = (x/factor, y/factor) }
          _ -> world


resX, resY :: Int
resX = 640
resY = 480
resWidth  = resX `div` 2
resHeight = resY `div` 2

factor :: Float
factor = 1
