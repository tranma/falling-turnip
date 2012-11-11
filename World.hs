{-# LANGUAGE ViewPatterns #-}
module World 
       ( Element (..), Cell (..)
       , Env (..)
       , Weight (..), WeightEnv (..)
       
       , nothing, turnip, steam_water, steam_condensed, fire, fire_end, oil
       , water, salt_water, sand, salt, stone, torch, plant, spout, metal, wall, lava

       , isFluid, isWall, isFire  
       , weight, age

       , MargPos (..)
       , GlossCoord (..), World (..)
       , resX, resY, winX, winY, resWidth, resHeight, palletteH
       , factor
       , render, outOfWorld, elemOf, tooltipFiles ) 
where

import Graphics.Gloss    
import Data.Word
import Data.Array.Repa (Z (..), (:.) (..), D, U, DIM2, Array)
import Data.Array.Repa.Repr.Vector
import qualified Data.Array.Repa.Eval   as R
import qualified Data.Array.Repa        as R

import Data.List


-- Basic constructs ------------------------------------------------------------

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
                   , nextGravityMask :: Array U DIM2 MargPos 
                   , tooltipLeft     :: Array V DIM2 Color 
                   , tooltipRight    :: Array V DIM2 Color }


-- Elements and properties -----------------------------------------------------

{-# INLINE nothing #-}
-- Must match on direct values for efficiency
nothing         = 0
steam_water     = 1
steam_condensed = 2
oil             = 6 
water           = 7
salt_water      = 8
sand            = 9
salt            = 10
stone           = 11
fire            = 12
fire_end        = 22
torch           = 23
plant           = 24
spout           = 25
metal           = 26
lava            = 27
turnip          = 126
wall            = 127

elems :: [Element]
elems = [ nothing        
        , steam_water    
        , steam_condensed
        , oil            
        , water          
        , salt_water     
        , sand           
        , salt           
        , stone          
        , fire           
        , fire_end       
        , torch          
        , plant          
        , spout          
        , metal          
        , lava           
        , turnip ] 

{-# INLINE isWall #-}
isWall :: Element -> Bool
isWall 23  = True     -- torch
isWall 24  = True     -- plant
isWall 25  = True     -- spout
isWall 26  = True     -- metal
isWall 127 = True     -- wall
isWall _   = False

{-# INLINE isFire #-}
isFire :: Element -> Bool
isFire x = x >= fire && x <= fire_end

{-# INLINE isFluid #-}
isFluid :: Element -> Element
isFluid 0  = 0     -- nothing
isFluid 1  = 0x40  -- steam
isFluid 2  = 0x40 
isFluid 6  = 0x40  -- oil
isFluid 7  = 0x40  -- water
isFluid 8  = 0x40  -- salt water
isFluid 27 = 0x40  -- lav
isFluid _  = 0

{-# INLINE weight #-}
weight :: Element -> Weight
weight 0  = 2      -- nothing
weight 1  = 0      -- steam water
weight 2  = 0      -- steam water
weight 9  = salt   -- sand == salt
weight 27 = water  -- lava == water
weight x | isFire x  = 0
         | otherwise = fromIntegral x

{-# INLINE age #-}
age :: Int -> Element -> Element
age r x 
  -- fire eventually goes out
  | x == fire_end = nothing
  | isFire x      = if r < 50 then x + 1 else x
  -- steam eventually condenses
  | x == steam_water     = if r < 1 then water else steam_water
  | x == steam_condensed = if r < 5 then water else steam_condensed
  -- turnip being turnip
  | x == turnip   = elems !! ((r * length elems) `div` 110)
  | otherwise     = x


-- Drawing ---------------------------------------------------------------------

render :: World -> Array D DIM2 Color
render world 
  = R.transpose $ (R.transpose $ tooltipLeft world R.++ R.map (dim . dim) (tooltipRight world)) 
             R.++ (R.transpose buttons)
             R.++ (R.transpose $ R.map colour $ array world) 
             


colour :: Element -> Color
colour 0   = black                                   -- nothing
colour 1   = bright $ light $ light $ light blue     -- steam           
colour 2   = bright $ light $ light $ light blue     -- steam condensed           
colour 6   = dark $ dim $ dim orange                 -- oil    
colour 7   = bright $ bright $ light blue            -- water  
colour 8   = bright $ bright $ light $ light blue    -- salt water
colour 9   = dim yellow                              -- sand   
colour 10  = greyN 0.95                              -- salt   
colour 11  = greyN 0.7                               -- stone  
colour 23  = bright $ orange                         -- torch
colour 24  = dim $ green                             -- plant
colour 25  = blue                                    -- spout
colour 26  = mixColors (0.2) (0.8) blue (greyN 0.5)  -- metal
colour 27  = bright red                              -- lava
colour 126 = violet                                  -- turnip
colour 127 = greyN 0.4                               -- wall   
colour x                                             -- fire
  | isFire x  = mixColors (1.0 * fromIntegral (x - fire)) 
                          (1.0 * fromIntegral (fire_end - x)) 
                          red yellow 
  | otherwise = error "render: element doesn't exist"


buttons :: Array V DIM2 Color
buttons = R.fromList (Z :. buttonH + paddingH :. resX) 
        $  hPadding  ++ hPadding2
       ++ (concat $ map oneLine [1..buttonH])
       ++  hPadding2 ++ hPadding
  where -- background
        bgUI      = black
        -- gap between buttons
        gap       = replicate gapSize bgUI        
        -- gap from left and right edges of the window
        side      = replicate sideSize bgUI
        -- gap from top and bottom of the palette
        hPadding  = replicate resX white
        hPadding2 = replicate resX bgUI
        -- one button
        oneBox e  = oneBox' $ colour e
        oneBox' c = replicate buttonW c
        -- one line = fire + rest of elements
        oneLine x 
          = let col = mixColors (fromIntegral x / fromIntegral buttonH) 
                                (1.0 - fromIntegral x / fromIntegral buttonH)
                                red yellow
            in  side ++ (concat $ intersperse gap $ oneBox' col : map oneBox selectableElems) ++ side

selectableElems :: [Element]
selectableElems = [ wall, nothing, turnip
                  , oil, water, sand, salt, stone
                  , torch, plant, spout, metal, lava ]

resX, resY, resWidth, resHeight, paddingH, tooltipH, gapSize, sideSize, buttonW, buttonH :: Int
-- size of the world
resX      = 320
resY      = 240
-- size of window = size of world + size of palette + size of tooltip area
winX      = resX
winY      = resY + buttonH + paddingH + tooltipH
-- gloss origin is at center, while repa origin is bottom left, so shifting needed
resWidth  = resX `div` 2
resHeight = resY `div` 2
-- 2 (top & bottom) * number of hPadding's
paddingH  = 4
-- size of buttons, tooltips and gaps
tooltipH  = 15
gapSize   = 2  
sideSize 
  = let n = 1 + length selectableElems
    in  (resX - n * buttonW - (n - 1) * gapSize) `div` 2
buttonW 
  = let n = 1 + length selectableElems
    in  (resX - (n-1)*gapSize) `div` n
buttonH   = 15


factor, palletteH :: Float
factor = 2
palletteH = (fromIntegral buttonH + fromIntegral paddingH + fromIntegral tooltipH)/2

outOfWorld :: GlossCoord -> Bool
outOfWorld (_, y) = round y + resHeight < 0

elemOf :: GlossCoord -> Element
elemOf ((subtract 5) . (+ resWidth) . round -> x, _)
  | x < buttonW               = fire
  | x <      gapSize + 2  * buttonW = wall
  | x < 2  * gapSize + 3  * buttonW = nothing
  | x < 3  * gapSize + 4  * buttonW = turnip
  | x < 4  * gapSize + 5  * buttonW = oil
  | x < 5  * gapSize + 6  * buttonW = water
  | x < 6  * gapSize + 7  * buttonW = sand
  | x < 7  * gapSize + 8  * buttonW = salt
  | x < 8  * gapSize + 9  * buttonW = stone
  | x < 9  * gapSize + 10 * buttonW = torch
  | x < 10 * gapSize + 11 * buttonW = plant 
  | x < 11 * gapSize + 12 * buttonW = spout
  | x < 12 * gapSize + 13 * buttonW = metal
  | otherwise                       = lava  

tooltipFiles =[(fire    , "tooltips/fire.png"),
               (wall    , "tooltips/wall.png"),
               (nothing , "tooltips/erase.png"),
               (oil     , "tooltips/oil.png"),
               (water   , "tooltips/water.png"),
               (sand    , "tooltips/sand.png"),
               (salt    , "tooltips/salt.png"),
               (stone   , "tooltips/stone.png"),
               (torch   , "tooltips/torch.png"),
               (plant   , "tooltips/plant.png"),
               (spout   , "tooltips/spout.png"),
               (metal   , "tooltips/metal.png"),
               (lava    , "tooltips/lava.png"),
               (turnip  , "tooltips/turnip.png")]
