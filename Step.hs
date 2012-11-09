module Step
     ( step
     , margMaskEven, margMaskOdd )
where

-- Repa
import Data.Array.Repa (Z (..), (:.) (..), U, D, DIM2, Array)
import Data.Array.Repa.Stencil
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.Unboxed    as R
import qualified Data.Array.Repa.Stencil.Dim2    as R

-- base
import Data.Bits

-- friends
import World
import Gravity


{-# INLINE step #-}
step :: Array U DIM2 MargPos -> Array U DIM2 Cell -> Array D DIM2 Cell
step mask array
  = let -- Repalce each cell with the collective value of all cells in its Margolus neighbourhood
        --    paired with the its position in the neighbourhood
        envs       = R.mapStencil2 (BoundFixed (nothing, 0)) margStencil $ R.zip array mask
        -- Apply gravity
    in  R.zipWith mkCell envs $ R.map (gravityRules . weigh) envs              
    where mkCell (env,p) pos 
            = let new = flip shiftR (8 * pos) $ env .&. margQuadrant pos
                  old = flip shiftR (8 * p)   $ env .&. margQuadrant p
              in if (isWall new || isWall old) then old else new
          margQuadrant pos = shiftL 0xff (8 * pos)          


-- Margolus block --------------------------------------------------------------

-- | Position of cells in a block automaton
--   0 1 0 1 ....
--   2 3 2 3 ....
--   ...
margMaskEven :: Array U DIM2 MargPos
margMaskEven
  = R.computeS $ R.fromFunction (Z:. resY :. resX)
               $ \(Z:. y :. x) -> x `mod` 2 .|. shiftL (y `mod` 2) 1

margMaskOdd :: Array U DIM2 MargPos
margMaskOdd = R.computeS $ R.map (flip subtract 3) margMaskEven

-- | Given a Moore neighbourhood (3x3), find the Margolus neighbourhood (2x2)
--    and encode it as a number, combined with the Margolus position for each cell
--
{-# INLINE margStencil #-}
margStencil :: Stencil DIM2 (Env, MargPos)
margStencil = StencilStatic (Z :. 3 :. 3) (0, -1) mkBlock
  where mkBlock :: DIM2 -> (Element, MargPos) -> (Env, MargPos) -> (Env, MargPos)
        mkBlock (Z :.  1 :. -1) (n,0) (acc, p) = (acc .|. n, p)
        mkBlock (Z :.  1 :.  0) (n,0) (acc, p) = (acc .|. n, p)
        mkBlock (Z :.  0 :. -1) (n,0) (acc, p) = (acc .|. n, p)
        mkBlock (Z :.  0 :.  0) (n,0) (acc, p) = (acc .|. n, 0)
        mkBlock (Z :.  1 :.  0) (n,1) (acc, p) = (acc .|. shiftL n 8, p)
        mkBlock (Z :.  1 :.  1) (n,1) (acc, p) = (acc .|. shiftL n 8, p)
        mkBlock (Z :.  0 :.  0) (n,1) (acc, p) = (acc .|. shiftL n 8, 1)
        mkBlock (Z :.  0 :.  1) (n,1) (acc, p) = (acc .|. shiftL n 8, p)
        mkBlock (Z :.  0 :. -1) (n,2) (acc, p) = (acc .|. shiftL n 16, p)
        mkBlock (Z :.  0 :.  0) (n,2) (acc, p) = (acc .|. shiftL n 16, 2)
        mkBlock (Z :. -1 :. -1) (n,2) (acc, p) = (acc .|. shiftL n 16, p)
        mkBlock (Z :. -1 :.  0) (n,2) (acc, p) = (acc .|. shiftL n 16, p)
        mkBlock (Z :.  0 :.  0) (n,3) (acc, p) = (acc .|. shiftL n 24, 3)
        mkBlock (Z :.  0 :.  1) (n,3) (acc, p) = (acc .|. shiftL n 24, p)
        mkBlock (Z :. -1 :.  0) (n,3) (acc, p) = (acc .|. shiftL n 24, p)
        mkBlock (Z :. -1 :.  1) (n,3) (acc, p) = (acc .|. shiftL n 24, p)
        mkBlock _ _ acc = acc
