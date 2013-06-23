{-# LANGUAGE QuasiQuotes #-}

import Data.Array.Repa (Z (..), (:.) (..), D, DIM2, Array, (!))
import qualified Data.Array.Repa                 as R

import Data.Array.Accelerate.IO
import Data.Array.Accelerate ((==*), (<*), (>*), (||*), Acc, Exp, (?))
import qualified Data.Array.Accelerate           as A
import qualified Data.Array.Accelerate.CUDA      as C

import Graphics.Gloss
import Graphics.Gloss.Raster.Array
import Graphics.Gloss.Interface.Pure.Game

type Life = Int
type Coord = (Float, Float)
type Matrix x = A.Array A.DIM2 x

data World = World { arr :: Array A DIM2 Life
                   , mouseDown :: Bool
                   , mousePos  :: Coord }

conwayStencil :: Acc (Matrix Life) -> Acc (Matrix Life)
conwayStencil = A.stencil f A.Clamp
  where f :: A.Stencil3x3 Life -> Exp Life
        f ((x00,x01,x02)
          ,(x10,x11,x12)
          ,(x20,x21,x22)) = x00 + x01 + x02 + x10 + x12 + x20 + x21 + x22

delta :: Exp Life -> Exp Int -> Exp Life
delta x n = (x ==* 1) ? ( (n <* 2 ||* n >* 3) ? (0, 1)
                        , (n ==* 3)           ? (1, 0))

tick :: Acc (Matrix Life) -> Acc (Matrix Life)
tick a = A.zipWith delta a $ conwayStencil a

resX = 640 
resY = 480

step :: Monad m => Float -> World -> m World
step time world
 = let updateMouse = if mouseDown world
                     then updatePoint (mousePos world) 1
                     else return
   in  do w' <- updateMouse (arr world)
          return $ world { arr = toRepa $ C.run $ tick $ A.lift $ fromRepa w' }

updatePoint :: Monad m => Coord -> Life -> Array A DIM2 Life -> m (Array A DIM2 Life)
updatePoint (x,y) newLife arr
 = let same a a' n = abs (round a + (n `div` 2) - a') < 5
   in  computeAccP $ R.fromFunction (Z :. resY :. resX)
              (\sh@(Z :. y' :. x')
                -> if same x x' resX && same y y' resY
                   then newLife
                   else arr ! sh)

handleInput :: Event -> World -> World
handleInput e world = case e of
  EventKey (MouseButton LeftButton) Down _ (x,y) -> world { mouseDown = True, mousePos = (x,y) }
  EventKey (MouseButton LeftButton) Up _   (x,y) -> world { mouseDown = False, mousePos = (x,y) }
  EventMotion (x,y) -> world { mousePos = (x,y) }

render :: World -> Array D DIM2 Color
render world = R.map color $ arr world
  where color 0 = black
        color _ = white

main :: IO ()
main = playArrayIO (InWindow "Conway" (resX,resY) (50,50))
                 (1,1)
                 10
                 (World { arr = R.computeS $ R.fromFunction (Z :. resY :. resX) foo
                        , mouseDown = False
                        , mousePos  = (0,0) })
                 (return . render)
                 ((return .) . handleInput)
                 step

foo sh@(Z :. y :. x) = if (x `div` (y + 1)) == 0 then 0 else 1
