{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

-- Repa
import Data.Array.Repa (Z (..), (:.) (..))
import qualified Data.Array.Repa                 as R

-- Gloss
import Graphics.Gloss              
import Graphics.Gloss.Raster.Array 
import Graphics.Gloss.Interface.Pure.Game  

-- base
import Control.Monad

-- friends
import World
import Step
import Draw

main :: IO ()                     
main = playArrayIO
  (InWindow "Falling Turnip" (resX * round factor, resY * round factor) (pos, pos))
  (round factor, round factor)
  frameRate
  (World { array = R.computeS $ R.fromFunction (Z :. resY :. resX) bareWorld
         , currentElem     = nothing
         , currGravityMask = margMaskEven
         , nextGravityMask = margMaskOdd
         , mouseDown       = False
         , mousePos        = (0,0) 
         , mousePrevPos    = (0,0) }) 
  ( return    . render)
  ((return .) . handleInput)
  stepWorld
  where frameRate = 60
        pos       = 300
        bareWorld = const nothing


handleInput :: Event -> World -> World
handleInput e w = handleInput' (w {mousePrevPos = mousePos w})
  where handleInput' world = case e of
          EventKey (MouseButton LeftButton) Down _ (x,y) -> world { mouseDown = True, mousePos = (x/factor, y/factor) }
          EventKey (MouseButton LeftButton) Up _   (x,y) -> world { mouseDown = False, mousePos = (x/factor, y/factor) }
          EventKey (Char 'e') Down _ _ -> world { currentElem = steam_water }
          EventKey (Char 'f') Down _ _ -> world { currentElem = fire        }
          EventKey (Char 'o') Down _ _ -> world { currentElem = oil         }
          EventKey (Char 'w') Down _ _ -> world { currentElem = water       }
          EventKey (Char 'l') Down _ _ -> world { currentElem = salt_water  }
          EventKey (Char 's') Down _ _ -> world { currentElem = sand        }
          EventKey (Char 'n') Down _ _ -> world { currentElem = salt        }
          EventKey (Char 't') Down _ _ -> world { currentElem = stone       }
          EventKey (Char 'a') Down _ _ -> world { currentElem = wall        }
          EventMotion (x,y) -> world { mousePos = (x/factor, y/factor) }
          _ -> world


stepWorld :: Float -> World -> IO World
stepWorld time world
 = do gravitised <- if mouseDown world 
                    then liftM   (step $ currGravityMask world)
                                $ drawLine (mousePrevPos world) (mousePos world) 
                                           (currentElem  world) (array    world)
                    else return $ step (currGravityMask world) $ array world
      array' <- R.computeP gravitised
      return $ world { array = array'
                     , currGravityMask = nextGravityMask world
                     , nextGravityMask = currGravityMask world } 

