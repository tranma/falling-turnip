{-# LANGUAGE QuasiQuotes #-}

module Gravity 
     ( stepGravity
     , margMaskEven, margMaskOdd )
where

import Data.Array.Repa (Z (..), (:.) (..), U, D, DIM2, Array)
import Data.Array.Repa.Stencil
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.Unboxed    as R
import qualified Data.Array.Repa.Stencil.Dim2    as R

import Data.Word
import Data.Bits
import World

type Env       = Word32
type WeightEnv = Word32


{-# INLINE stepGravity #-}
stepGravity :: Array U DIM2 MargPos -> Array U DIM2 Cell -> Array D DIM2 Cell
stepGravity mask array
  = let -- Repalce each cell with the collective value of all cells in its Margolus neighbourhood
        --    paired with the its position in the neighbourhood
        envs       = R.mapStencil2 (BoundFixed (nothing, 0)) gravityStencil $ R.zip array mask
        -- Apply gravity
     in R.zipWith mkCell envs $ R.map (gravityRules . weigh) envs              
    where mkCell (env,p) pos = let new = flip shiftR (8 * pos) $ env .&. margQuadrant pos
                                   old = flip shiftR (8 * p) $ env .&. margQuadrant p
                               in if (isWall new || isWall old) then old else new
          margQuadrant pos = shiftL 0xff (8 * pos)          

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
{-# INLINE gravityStencil #-}
gravityStencil :: Stencil DIM2 (Env, MargPos)
gravityStencil = StencilStatic (Z :. 3 :. 3) (0, -1) mkBlock
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
        --allNothing = nothing .|. shiftL nothing 8 .|. shiftL nothing 16 .|. shiftL nothing 24


--{-# INLINE weigh #-}
weigh :: (Env, MargPos) -> WeightEnv
weigh (env, pos)
  = let -- Break up the environment into its four components
        ul' = (env .&. eight1)
        ur' = (flip shiftR 8 $ env .&. eight2)
        dl' = (flip shiftR 16 $ env .&. eight3)
        dr' = (flip shiftR 24 $ env .&. eight4)
        -- Determine the heaviest item in the environment
        heaviest = max (max ul' ur') (max dl' dr')
        -- Compare each cell with the heaviest, lowest bit set if >=        
        ul, ur, dl, dr :: Weight
        ul = (0x80 .&. (heaviest - 1 - weight ul')) .|. isFluid ul'
        ur = (0x80 .&. (heaviest - 1 - weight ur')) .|. isFluid ur'
        dl = (0x80 .&. (heaviest - 1 - weight dl')) .|. isFluid dl'
        dr = (0x80 .&. (heaviest - 1 - weight dr')) .|. isFluid dr'
        -- Combine the lighter/heavier state of all 4 cells into an env
        --  32bits: | DR | DL | UR | UL |
        wenv = ul .|. (shiftL ur 8) .|. (shiftL dl 16) .|. (shiftL dr 24)
        -- Mark the current one
    in wenv .|. shiftL 1 (8 * pos)
    where eight1 = 0xff
          eight2 = shiftL eight1 8
          eight3 = shiftL eight2 8
          eight4 = shiftL eight3 8

-- Possible values:
-- 
--  L liquid      C0
--  L liq, focus  C1
--  ~ liq, space  40
--  ~ liqspace f  41
--
--  * non-focused 80
--  * focused     81
--  ~ non-focused 00
--  ~ focused     01
gravityRules :: WeightEnv -> MargPos
gravityRules wenv = case wenv of 
  -- L ~ --> ~ L
  -- * *     * *
  0x808000C1 -> 1
  0x808001C0 -> 0
  0x808040C1 -> 1
  0x808041C0 -> 0
  0x80C000C1 -> 1
  0x80C001C0 -> 0
  0x80C040C1 -> 1
  0x80C041C0 -> 0
  0xC08000C1 -> 1
  0xC08001C0 -> 0
  0xC08040C1 -> 1
  0xC08041C0 -> 0
  0xC0C000C1 -> 1
  0xC0C001C0 -> 0
  0xC0C040C1 -> 1
  0xC0C041C0 -> 0 
  -- ~ L --> L ~
  -- * *     * *
  0x8080C100 -> 0
  0x8080C001 -> 1
  0x8080C140 -> 0
  0x8080C041 -> 1
  0x80C0C100 -> 0
  0x80C0C001 -> 1
  0x80C0C140 -> 0
  0x80C0C041 -> 1
  0xC080C100 -> 0
  0xC080C001 -> 1
  0xC080C140 -> 0
  0xC080C041 -> 1
  0xC0C0C100 -> 0
  0xC0C0C001 -> 1
  0xC0C0C140 -> 0
  0xC0C0C041 -> 1 
  -- ~ ~ --> ~ ~
  -- L ~     ~ L
  0x00C10000 -> 3
  0x01C00000 -> 2
  0x40C10000 -> 3
  0x41C00000 -> 2 
  0x00C14000 -> 3
  0x01C04000 -> 2 
  0x40C14000 -> 3
  0x41C04000 -> 2
  0x00C10040 -> 3
  0x01C00040 -> 2
  0x40C10040 -> 3
  0x41C00040 -> 2
  0x00C14040 -> 3
  0x01C04040 -> 2
  0x40C14040 -> 3
  0x41C04040 -> 2
  -- ~ ~ --> ~ ~
  -- ~ L     L ~
  0xC1000000 -> 2
  0xC0010000 -> 3
  0xC1400000 -> 2
  0xC0410000 -> 3
  0xC1004000 -> 2
  0xC0014000 -> 3
  0xC1404000 -> 2
  0xC0414000 -> 3
  0xC1000040 -> 2
  0xC0010040 -> 3
  0xC1400040 -> 2
  0xC0410040 -> 3
  0xC1004040 -> 2
  0xC0014040 -> 3
  0xC1404040 -> 2
  0xC0414040 -> 3
  _ -> case (wenv .&. 0x81818181) of
    -- * ~ --> ~ ~
    -- ~ ~     * ~
    0x00000081 -> 2
    0x00010080 -> 0
    -- * * --> * ~
    -- * ~     * *
    0x00808180 -> 3
    0x01808080 -> 1
    -- * * --> ~ ~
    -- ~ ~     * *
    0x00008081 -> 2
    0x00008180 -> 3
    0x00018080 -> 0
    0x01008080 -> 1
    -- ~ * --> ~ ~
    -- * ~     * *
    0x00808100 -> 3
    0x01808000 -> 1
    -- ~ * --> ~ ~
    -- ~ ~     ~ *
    0x00008100 -> 3
    0x01008000 -> 1
    -- * * --> ~ *
    -- ~ *     * *
    0x80008081 -> 2
    0x80018080 -> 0
    -- * ~ --> ~ ~
    -- ~ *     * *
    0x80000081 -> 2
    0x80010080 -> 0
    -- * ~ --> ~ ~
    -- * ~     * *
    0x00800081 -> 3
    0x01800080 -> 0
    -- ~ * --> ~ ~
    -- ~ *     * *
    0x80008100 -> 2
    0x80018000 -> 1

    x -> case x .&. 0x01010101 of
      0x01000000 -> 3
      0x00010000 -> 2
      0x00000100 -> 1
      0x00000001 -> 0

