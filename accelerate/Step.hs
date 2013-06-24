module Repa.Step
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


-- friends
import Common.World
import Repa.Gravity
import Repa.Alchemy

{-# INLINE step #-}
step :: Int -> Acc (Matrix MargPos) -> Acc (Matrix Cell) -> Acc (Matrix Cell)
step gen mask array
  = let randomish = randomArray (uniformR (0, 100)) (Z :. resY :. resX)
        envs = A.stencil2 R.Clamp array R.Clamp mask

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
{-# INLINE combine #-}
combine :: (Cell, Cell, Cell, Cell) -> Env
combine (ul, ur, dl, dr)
 = ul .|. (shiftL ur 8) .|. (shiftL dl 16) .|. (shiftL dr 24)

{-# INLINE combine' #-}
combine' :: (Weight, Weight, Weight, Weight) -> WeightEnv
combine' (ul, ur, dl, dr)
 = ul .|. (shiftL ur 2) .|. (shiftL dl 4) .|. (shiftL dr 6)


-- | Apply gravity to the cell at quadrant 'pos' in 'env'
--      returning the quadrant it should swap with
{-# INLINE weigh #-}
weigh :: (Env, MargPos) -> MargPos
weigh (env, pos)
  = let current              = margQuadrant pos env
        (ul', ur', dl', dr') = split env

        -- The heaviest item in the environment
        heaviest = max (max (weight ul') (weight ur'))
                       (max (weight dl') (weight dr'))


        -- Compare each cell with the heaviest, lowest bit set if >=
        ul, ur, dl, dr :: Weight
        ul = (if (weight ul' >= heaviest) then 1 else 0) .|. isFluid ul'
        ur = (if (weight ur' >= heaviest) then 1 else 0) .|. isFluid ur'
        dl = (if (weight dl' >= heaviest) then 1 else 0) .|. isFluid dl'
        dr = (if (weight dr' >= heaviest) then 1 else 0) .|. isFluid dr'
        weighed1 = combine' (ul, ur, dl, dr)

        -- Apply gravity with respect to the heaviest
        x' =  applyGravity weighed1 pos -- .|. shiftL 1 (8 * pos))

        x  = if isWall (margQuadrant x' env) then pos else x'

        -- The second heaviest item
        remainingWeights
          = filter (/= heaviest)
                   [weight ul', weight ur', weight dl', weight dr']
        nextHeaviest = maximum $ remainingWeights

        -- Compare each cell with the second heaviest, lowest bit set if >=
        ul2, ur2, dl2, dr2 :: Weight
        ul2 = (if (weight ul' >= nextHeaviest) then 1 else 0) .|. isFluid ul'
        ur2 = (if (weight ur' >= nextHeaviest) then 1 else 0) .|. isFluid ur'
        dl2 = (if (weight dl' >= nextHeaviest) then 1 else 0) .|. isFluid dl'
        dr2 = (if (weight dr' >= nextHeaviest) then 1 else 0) .|. isFluid dr'
        weighed2 = combine' (ul2, ur2, dl2, dr2)

        -- Apply gravity with respect to the second heaviest
        y' =  applyGravity weighed2  pos
        y  = if isWall (margQuadrant y' env) then pos else y'

        -- Compose the two gravity passes
        ydest' =  applyGravity (weighed1) y
        ydest = if isWall (margQuadrant ydest' env) then y else ydest'

    in if      (ul' == ur' && ur' == dl' && dl' == dr')   then pos
       else if (isWall current)                           then pos
       else if x /= pos || (length remainingWeights <= 1) then x
       else if ydest == y                                 then y
       else x


-- | Perform alchemy on a margolus block, with randomised probability of succeeding
{-# INLINE alchemy #-}
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
{-# INLINE margMaskEven #-}
margMaskEven :: Array U DIM2 MargPos
margMaskEven
  = R.computeS $ R.fromFunction (Z:. resY :. resX)
               $ \(Z:. y :. x) -> x `mod` 2 .|. shiftL (y `mod` 2) 1

{-# INLINE margMaskOdd #-}
margMaskOdd :: Array U DIM2 MargPos
margMaskOdd = R.computeS $ R.map (flip subtract 3) margMaskEven

-- | Given a Moore neighbourhood (3x3), find the Margolus neighbourhood (2x2)
--    and encode it as a number, combined with the Margolus position for each cell
--
{-# INLINE margStencil #-}
margStencil :: A.Stencil3x3 Cell -> Exp Cell
margStencil ((_, (y0x1',2), (y1x1',0))
            ,(_, (y0x0 ,3), (y1x0 ,1))
            ,(_, _        , _       ))
            = ((((y1x1' .|. shiftL n 8 y1x0) .|. shiftL n 16 y0x1') .|. shiftL n 32 y0x0), 3)

margStencil ((_, _       , _      )
            ,(_, (y0x0,2), (y1x0,0))
            ,(_, (y0x1,3), (y1x1,1)))
            = ((((y1x0 .|. shiftL n 8 y1x1) .|. shiftL n 16 y0x0) .|. shiftL n 32 y0x1), 2)

margStencil ((_, (y0x1',0), (y1'x1',2))
            ,(_, (y0x0 ,1), (y1'x0 ,3))
            ,(_, _        , _        ))
            = ((((y0x1' .|. shiftL n 8 y0x0) .|. shiftL n 16 y1'x1') .|. shiftL n 32 y1'x0), 1)

margStencil ((_, _       , _        )
            ,(_, (y0x0,0), (y1'x0,2))
            ,(_, (y0x1,1), (y1'x1,3)))
            = ((((y0x0 .|. shiftL n 8 y0x1) .|. shiftL n 16 y1'x0) .|. shiftL n 32 y1'x1), 0)
