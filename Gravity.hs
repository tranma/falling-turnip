{-# LANGUAGE QuasiQuotes #-}

module Gravity 
       (stepGravity)
where

import Data.Array.Repa (Z (..), (:.) (..), D, U, DIM2, Array)
import Data.Array.Repa.Stencil
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Stencil         as R
import qualified Data.Array.Repa.Stencil.Dim2    as R

import Data.Word
import Data.Bits

import World


type Env         = Word64
type ExpandedEnv = (Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell)


{-# INLINE stepGravity #-}
stepGravity :: Bool -> Array D DIM2 Cell -> IO (Array U DIM2 Cell)
stepGravity isOdd x 
  = let gravity = if isOdd then applyGravity oddRules else applyGravity evenRules
    in  R.computeP $ R.map gravity
                   $ R.mapStencil2 (BoundConst 0) gravityStencil x

{-# INLINE gravityStencil #-}
gravityStencil :: Stencil DIM2 Cell
gravityStencil 
  = R.StencilStatic (Z :. 3 :. 3) 0
    $ \ix val acc -> case ix of
      (Z :. -1 :. -1) -> acc .|. val
      (Z :. -1 :.  0) -> acc .|. shiftL val 7
      (Z :. -1 :.  1) -> acc .|. shiftL val 14
      (Z :.  0 :. -1) -> acc .|. shiftL val 21
      (Z :.  0 :.  0) -> acc .|. shiftL val 28
      (Z :.  0 :.  1) -> acc .|. shiftL val 35
      (Z :.  1 :. -1) -> acc .|. shiftL val 42
      (Z :.  1 :.  0) -> acc .|. shiftL val 49
      (Z :.  1 :.  1) -> acc .|. shiftL val 56
      s               -> acc


-- Rules -----------------------------------------------------------------------

{-# INLINE heavier #-}
heavier :: Cell -> Cell -> Bool
heavier x y = x < y 

{-# INLINE movable #-}
movable :: Element -> Bool
movable 100 = False
movable _   = True

{-# INLINE applyGravity #-}
applyGravity :: (ExpandedEnv -> Cell) -> Env -> Cell 
applyGravity rules env
 = let  seven1 = 0x7f
        seven2 = shiftL seven1 7
        seven3 = shiftL seven2 7
        seven4 = shiftL seven3 7
        seven5 = shiftL seven4 7
        seven6 = shiftL seven5 7
        seven7 = shiftL seven6 7
        seven8 = shiftL seven7 7
        seven9 = shiftL seven8 7
 
        topLeft  = (env .&. seven1)
        top      = flip shiftR 7  $ env .&. seven2
        topRight = flip shiftR 14 $ env .&. seven3
        left     = flip shiftR 21 $ env .&. seven4
        me       = flip shiftR 28 $ env .&. seven5
        right    = flip shiftR 35 $ env .&. seven6
        lowLeft  = flip shiftR 42 $ env .&. seven7
        low      = flip shiftR 49 $ env .&. seven8
        lowRight = flip shiftR 56 $ env .&. seven9
   in  if   movable me 
       then rules (topLeft, top, topRight, left, me, right, lowLeft, low, lowRight)
       else me

{-# INLINE oddRules #-}
oddRules :: ExpandedEnv -> Cell
oddRules (topLeft, top, topRight, left, me, right, lowLeft, low, lowRight)
  | movable top, top `heavier` me = top
  | movable low, me `heavier` low = low
  | movable topRight, topRight `heavier` me, me /= nothing = topRight
  | movable lowLeft, me `heavier` lowLeft, me /= nothing = lowLeft
  | otherwise = me

{-# INLINE evenRules #-}
evenRules :: ExpandedEnv -> Cell
evenRules (topLeft, top, topRight, left, me, right, lowLeft, low, lowRight)
  | movable top, top `heavier` me = top
  | movable low, me `heavier` low = low
  | movable topLeft, topLeft `heavier` me, me /= nothing = topLeft
  | movable lowRight, me `heavier` lowRight, me /= nothing = lowRight
  | otherwise = me
