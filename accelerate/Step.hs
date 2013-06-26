{-# LANGUAGE ViewPatterns #-}
module Accelerate.Step
     ( step
     , margMaskEven, margMaskOdd, weigh, combine )
where

-- Acc
import Data.Array.Accelerate (Acc, Exp, Z(..), (:.)(..), (?), (>=*), (&&*), (||*), (==*), (/=*))
import qualified Data.Array.Accelerate           as A

-- base
import Data.Bits

-- friends
import Accelerate.World
import Accelerate.Gravity
import Accelerate.Alchemy
--import Accelerate.World
import Random.Array

type Env4 = (Cell, Cell, Cell, Cell)

step :: Int -> Acc (Matrix MargPos) -> Acc (Matrix Cell) -> Acc (Matrix Cell)
step gen mask array
  = let randomish = A.use $ randomArray (uniformR (0, 100)) (Z:.resY:.resX)
        envs  = A.zipWith (\a x -> A.lift (alchemy a $ A.fst x, A.snd x)) randomish
              $ A.stencil margStencil A.Clamp
              $ A.zip array mask
    in  A.zipWith age randomish
      $ A.zipWith mkCell envs
      $ A.map weigh envs
    where -- Swap cell at position 'p' in the margolus block 'env' with
          --    the cell at 'pos' in the same block
          mkCell :: Exp (Env4, MargPos) -> Exp MargPos -> Exp Cell
          mkCell x pos = margQuadrant pos $ A.fst x


-- | Mask to extract cell at quadrant 'pos'

margQuadrant :: Exp MargPos -> Exp (Cell, Cell, Cell, Cell) -> Exp Cell
margQuadrant p (A.unlift -> (x0,x1,x2,x3)) = (p ==* 0) ? (x0
                                           , (p ==* 1) ? (x1
                                           , (p ==* 2) ? (x2
                                           , x3)))


-- | Break up the environment into its four components
{-
split :: Exp Env -> (Exp Cell, Exp Cell, Exp Cell, Exp Cell)
split env
 = let ul = (env .&. eight1)
       ur = (flip A.shiftR 8 $ env .&. eight2)
       dl = (flip A.shiftR 16 $ env .&. eight3)
       dr = (flip A.shiftR 24 $ env .&. eight4)
   in (ul, ur, dl, dr)
    where -- Masks for extracting 8-bit slices
          eight1 = 0xff
          eight2 = A.shiftL eight1 8
          eight3 = A.shiftL eight2 8
          eight4 = A.shiftL eight3 8
-}
-- | Combine the lighter/heavier state of all 4 cells into an env
--     32bits: | DR | DL | UR | UL |

combine :: (Exp Cell, Exp Cell, Exp Cell, Exp Cell) -> Exp Env
combine (ul, ur, dl, dr)
 = ul .|. (A.shiftL ur 8) .|. (A.shiftL dl 16) .|. (A.shiftL dr 24)


combine' :: (Exp Weight, Exp Weight, Exp Weight, Exp Weight) -> Exp WeightEnv
combine' (ul, ur, dl, dr)
 = ul .|. (A.shiftL ur 2) .|. (A.shiftL dl 4) .|. (A.shiftL dr 6)


-- | Apply gravity to the cell at quadrant 'pos' in 'env'
--      returning the quadrant it should swap with

weigh :: Exp (Env4, MargPos) -> Exp MargPos
weigh (A.unlift -> (env, pos))
  = let current              = margQuadrant pos env
        (ul', ur', dl', dr') = A.unlift env

        -- The heaviest item in the environment
        heaviest = A.max (A.max (weight ul') (weight ur'))
                         (A.max (weight dl') (weight dr'))


        -- Compare each cell with the heaviest, lowest bit set if >=
        ul, ur, dl, dr :: Exp Weight
        ul = ((weight ul' >=* heaviest) ? (1 , 0)) .|. isFluid ul'
        ur = ((weight ur' >=* heaviest) ? (1 , 0)) .|. isFluid ur'
        dl = ((weight dl' >=* heaviest) ? (1 , 0)) .|. isFluid dl'
        dr = ((weight dr' >=* heaviest) ? (1 , 0)) .|. isFluid dr'
        weighed1 = combine' (ul, ur, dl, dr)

        -- Apply gravity with respect to the heaviest
        x' =  applyGravity weighed1 pos

        x  = isWall (margQuadrant x' env) ? (pos, x')

        nextHeaviest4 :: (Exp Weight, Exp Weight, Exp Weight, Exp Weight)
                      -> (Exp Weight, Exp Weight, Exp Weight)
        nextHeaviest4 (a,b,c,d) = A.unlift ((a ==* heaviest) ? (A.lift (b,c,d)
                                , (b ==* heaviest) ? (A.lift (a,c,d)
                                , (c ==* heaviest) ? (A.lift (a,b,d)
                                , A.lift (a,b,c)))))
        next3 = nextHeaviest4 (weight ul',weight ur',weight dl',weight dr')
        maxOf3 (a,b,c) = A.max (A.max a b) c
        nextHeaviest' = maxOf3 next3

        nextHeaviest3 (a,b,c) = A.unlift ((a ==* nextHeaviest') ? (A.lift (b,c)
                              , (b ==* nextHeaviest') ? (A.lift (a,c)
                              , A.lift (a,b))))

        nextHeaviest = (nextHeaviest' ==* heaviest) 
                         ? (uncurry (A.max) (nextHeaviest3 next3), nextHeaviest')

        -- Compare each cell with the second heaviest, lowest bit set if >=
        ul2, ur2, dl2, dr2 :: Exp Weight
        ul2 = ((weight ul' >=* nextHeaviest) ? ( 1 , 0)) .|. isFluid ul'
        ur2 = ((weight ur' >=* nextHeaviest) ? ( 1 , 0)) .|. isFluid ur'
        dl2 = ((weight dl' >=* nextHeaviest) ? ( 1 , 0)) .|. isFluid dl'
        dr2 = ((weight dr' >=* nextHeaviest) ? ( 1 , 0)) .|. isFluid dr'
        weighed2 = combine' (ul2, ur2, dl2, dr2)

        -- Apply gravity with respect to the second heaviest
        y' =  applyGravity weighed2  pos
        y  = isWall (margQuadrant y' env) ? (pos , y')

        -- Compose the two gravity passes
        ydest' =  applyGravity (weighed1) y
        ydest = isWall (margQuadrant ydest' env) ? (y , ydest')

    in (ul' ==* ur' &&* ur' ==* dl' &&* dl' ==* dr') ? ( pos
     , (isWall current)                              ? ( pos
     , (x /=* pos ||* nextHeaviest ==* heaviest)     ? ( x
     , (ydest ==* y )                                ? ( y
                                                       , x))))

-- | Perform alchemy on a margolus block, with randomised probability of succeeding

alchemy :: Exp Int -> Exp Env4 -> Exp Env4
alchemy i env
 = let (ul0, ur0, dl0, dr0) = A.unlift env
       -- Apply interaction among the components
       (ul1, ur1) = unlift' $ applyAlchemy i ul0 ur0
       (ur , dr2) = unlift' $ applyAlchemy i ur1 dr0
       (dr , dl3) = unlift' $ applyAlchemy i dr2 dl0
       (dl , ul ) = unlift' $ applyAlchemy i dl3 ul1
   in  (ul0 ==* ur0 &&* ur0 ==* dl0 &&* dl0 ==* dr0)
       ? ( env
         , A.lift (ul, ur, dl, dr)
         )
 where unlift' :: Exp (Cell, Cell) -> (Exp Cell, Exp Cell)
       unlift' = A.unlift

-- Margolus block --------------------------------------------------------------

-- | Position of cells in a block automaton
--   0 1 0 1 ....
--   2 3 2 3 ....
--   ...

margMaskEven :: Acc (Matrix MargPos)
margMaskEven
  = A.generate res
          $ \ix -> let (Z:.y:.x) = A.unlift ix in x `mod` 2 .|. shiftL (y `mod` 2) 1


margMaskOdd :: Acc (Matrix MargPos)
margMaskOdd
  = A.map (flip subtract 3) margMaskEven

-- | Given a Moore neighbourhood (3x3), find the Margolus neighbourhood (2x2)
--    and encode it as a number, combined with the Margolus position for each cell
--

margStencil :: A.Stencil3x3 (Cell, MargPos) -> Exp ((Cell, Cell, Cell, Cell), MargPos)
margStencil ((y0x0,y0x1,y0x2)
            ,(y1x0,y1x1,y1x2)
            ,(y2x0,y2x1,y2x2))
            = A.lift $ flip (,) (A.snd y1x1) $
            ( (A.snd y1x1 A.==* 0) A.? (A.lift (A.fst y1x1, A.fst y1x2, A.fst y2x1, A.fst y2x2)
            , (A.snd y1x1 A.==* 1) A.? (A.lift (A.fst y1x0, A.fst y1x1, A.fst y2x0, A.fst y2x1)
            , (A.snd y1x1 A.==* 2) A.? (A.lift (A.fst y0x1, A.fst y0x2, A.fst y1x1, A.fst y1x2)
            ,                          (A.lift (A.fst y0x0, A.fst y0x1, A.fst y1x0, A.fst y1x1))))))



