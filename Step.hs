module Step
     ( step
     , margMaskEven, margMaskOdd, weigh, combine )
where

-- Repa
import Data.Array.Repa (Z (..), (:.) (..), U, D, DIM2, Array)
import Data.Array.Repa.Stencil
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.Unboxed    as R
import qualified Data.Array.Repa.Stencil.Dim2    as R
import Data.Array.Repa.Algorithms.Randomish      as R
-- base
import Data.Bits
import Control.Arrow


-- friends
import World
import Gravity
import Alchemy


step :: Int -> Array U DIM2 MargPos -> Array U DIM2 Cell -> Array D DIM2 Cell
step gen mask array
  = let randomish = R.randomishIntArray (Z :. resY :. resX) 0 100 gen 
        envs  = R.zipWith (\a (b,c) -> (alchemy a b, c)) randomish
              $ R.mapStencil2 (BoundFixed (nothing, 0)) margStencil 
              $ R.zip array mask
    in  R.zipWith age randomish 
      $ R.zipWith mkCell envs 
      $ R.map weigh envs
    where -- Swap cell at position 'p' in the margolus block 'env' with
          --    the cell at 'pos' in the same block
          mkCell (env,_) pos = margQuadrant pos env
               
          
-- | Mask to extract cell at quadrant 'pos'
{-# INLINE margQuadrant #-}
margQuadrant :: MargPos -> Env -> Cell          
margQuadrant pos = flip shiftR (8 * pos) . (.&. shiftL 0xff (8 * pos))


-- | Break up the environment into its four components
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

-- | Combine the lighter/heavier state of all 4 cells into an env
--     32bits: | DR | DL | UR | UL |
combine :: (Cell, Cell, Cell, Cell) -> Env
combine (ul, ur, dl, dr)
 = ul .|. (shiftL ur 8) .|. (shiftL dl 16) .|. (shiftL dr 24)


-- | Apply gravity to the cell at quadrant 'pos' in 'env'
--      returning the quadrant it should swap with
weigh :: (Env, MargPos) -> MargPos
weigh (env, pos)
  = let current              = margQuadrant pos env
        (ul', ur', dl', dr') = split env        

        -- The heaviest item in the environment
        heaviest = max (max (weight ul') (weight ur'))
                       (max (weight dl') (weight dr'))

        -- Compare each cell with the heaviest, lowest bit set if >=        
        ul, ur, dl, dr :: Weight
        ul = (0x80 .&. (heaviest - 1 - weight ul')) .|. isFluid ul'
        ur = (0x80 .&. (heaviest - 1 - weight ur')) .|. isFluid ur'
        dl = (0x80 .&. (heaviest - 1 - weight dl')) .|. isFluid dl'
        dr = (0x80 .&. (heaviest - 1 - weight dr')) .|. isFluid dr'
        weighed1 = combine (ul, ur, dl, dr)
        
        -- Apply gravity with respect to the heaviest
        x' = applyGravity (weighed1 .|. shiftL 1 (8 * pos))                
        x  = if isWall (margQuadrant x' env) then pos else x'

        -- The second heaviest item
        remainingWeights 
          = filter (/= heaviest)
                   [weight ul', weight ur', weight dl', weight dr']
        nextHeaviest = maximum $ remainingWeights 

        -- Compare each cell with the second heaviest, lowest bit set if >=          
        ul2, ur2, dl2, dr2 :: Weight
        ul2 = (0x80 .&. (nextHeaviest - 1 - weight ul')) .|. isFluid ul'
        ur2 = (0x80 .&. (nextHeaviest - 1 - weight ur')) .|. isFluid ur'
        dl2 = (0x80 .&. (nextHeaviest - 1 - weight dl')) .|. isFluid dl'
        dr2 = (0x80 .&. (nextHeaviest - 1 - weight dr')) .|. isFluid dr'
        weighed2 = combine (ul2, ur2, dl2, dr2)

        -- Apply gravity with respect to the second heaviest
        y' = applyGravity (weighed2 .|. shiftL 1 (8 * pos))
        y  = if isWall (margQuadrant y' env) then pos else y'

        -- Compose the two gravity passes 
        ydest' = applyGravity (weighed1 .|. shiftL 1 (8 * y))
        ydest = if isWall (margQuadrant ydest' env) then y else ydest'

    in if      (ul' == ur' && ur' == dl' && dl' == dr')   then pos 
       else if (isWall current)                           then pos 
       else if x /= pos || (length remainingWeights <= 1) then x 
       else if ydest == y                                 then y
       else x
      
        
-- | Perform alchemy on a margolus block, with randomised probability of succeeding
alchemy :: Int -> Env -> Env
alchemy i env
 = let (ul0, ur0, dl0, dr0) = split env
       -- Apply interaction among the components
       (ul1, ur1) = applyAlchemy i ul0 ur0
       (ur , dr2) = applyAlchemy i ur1 dr0
       (dr , dl3) = applyAlchemy i dr2 dl0
       (dl , ul ) = applyAlchemy i dl3 ul1
   in  if (ul0 == ur0 && ur0 == dl0 && dl0 == dr0) 
       then env 
       else combine (ul, ur, dl, dr)


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
