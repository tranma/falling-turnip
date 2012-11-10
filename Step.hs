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
import Control.Arrow

-- friends
import World
import Gravity
import Alchemy


step :: Array U DIM2 MargPos -> Array U DIM2 Cell -> Array D DIM2 Cell
step mask array
  = let envs  = R.map (first alchemy)
              $ R.mapStencil2 (BoundFixed (nothing, 0)) margStencil $ R.zip array mask
    in  R.zipWith mkCell envs $ R.map (applyGravity . weigh) envs
    where -- Swap cell at position 'p' in the margolus block 'env' with
          --    the cell at 'pos' in the same block
          mkCell (env,p) pos 
            = let new = flip shiftR (8 * pos) $ env .&. margQuadrant pos
                  old = flip shiftR (8 * p)   $ env .&. margQuadrant p
              in if (isWall new || isWall old) then old else new
          -- Mask to extract cell at quadrant 'pos'
          margQuadrant pos = shiftL 0xff (8 * pos) 


-- Break up the environment into its four components
{-# INLINE split #-}
split :: Env -> (Cell, Cell, Cell, Cell)
split env
 = let ul = (env .&. eight1)
       ur = (flip shiftR 8 $ env .&. eight2)
       dl = (flip shiftR 16 $ env .&. eight3)
       dr = (flip shiftR 24 $ env .&. eight4)
   in (ul, ur, dl, dr)
    where -- Masks for extracting 8-bit slices
          eight1 = 0xff
          eight2 = shiftL eight1 8
          eight3 = shiftL eight2 8
          eight4 = shiftL eight3 8


-- Combine the lighter/heavier state of all 4 cells into an env
--  32bits: | DR | DL | UR | UL |
{-# INLINE combine #-}
combine :: (Cell, Cell, Cell, Cell) -> Env
combine (ul, ur, dl, dr)
 = ul .|. (shiftL ur 8) .|. (shiftL dl 16) .|. (shiftL dr 24)


weigh :: (Env, MargPos) -> WeightEnv
weigh (env, pos)
  = let (ul', ur', dl', dr') = split env

        -- Determine the heaviest item in the environment
        heaviest = max (max ul' ur') (max dl' dr')
        
        -- Compare each cell with the heaviest, lowest bit set if >=        
        ul, ur, dl, dr :: Weight
        ul = (0x80 .&. (heaviest - 1 - weight ul')) .|. isFluid ul'
        ur = (0x80 .&. (heaviest - 1 - weight ur')) .|. isFluid ur'
        dl = (0x80 .&. (heaviest - 1 - weight dl')) .|. isFluid dl'
        dr = (0x80 .&. (heaviest - 1 - weight dr')) .|. isFluid dr'

        -- Mark the current one
    in  combine (ul, ur, dl, dr) .|. shiftL 1 (8 * pos)


alchemy :: Env -> Env
alchemy env
 = let (ul0, ur0, dl0, dr0) = split env
       -- Apply interaction among the components
       (ul1, ur1) = applyAlchemy ul0 ur0
       (ur , dr2) = applyAlchemy ur1 dr0
       (dr , dl3) = applyAlchemy dr2 dl0
       (dl , ul ) = applyAlchemy dl3 ul1
   in  combine (ul, ur, dl, dr)


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
