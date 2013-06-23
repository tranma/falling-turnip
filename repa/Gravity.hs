{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
module Repa.Gravity
      (applyGravity)
where

import Data.Bits
import Common.World
import Language.Literals.Binary
-- Black magic for gravity
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
--


applyGravity :: WeightEnv -> MargPos -> MargPos
applyGravity x n = let (a,b,c,d) = applyGravity' x
                    in case n of 0 -> a; 1 -> b; 2 -> c; 3 -> d

{-# INLINE ignoreL #-}
ignoreL :: MargPos -> WeightEnv -> WeightEnv
ignoreL 0 x = x .&. [b|11 11 11 01|]
ignoreL 1 x = x .&. [b|11 11 01 11|]
ignoreL 2 x = x .&. [b|11 01 11 11|]
ignoreL 3 x = x .&. [b|01 11 11 11|]

{-# INLINE applyGravity' #-}
applyGravity' :: WeightEnv -> (MargPos, MargPos, MargPos, MargPos)
applyGravity' wenv = case wenv of
  -- L L --> L L
  -- L ~     ~ L
  (ignoreL 3 -> [b|00 11 11 11|]) -> (0,1,3,2)
  -- L L --> L L
  -- ~ L     L ~
  (ignoreL 2 -> [b|11 00 11 11|]) -> (0,1,3,2)
  -- L ~ --> ~ L
  -- * *     * *
  (ignoreL 1 . ignoreL 2 . ignoreL 3 -> [b|01 01 00 11|]) -> (1,0,2,3)
  -- ~ L --> L ~
  -- * *     * *
  (ignoreL 0 . ignoreL 2 . ignoreL 3 -> [b|01 01 11 00|]) -> (1,0,2,3)
  -- ~ ~ --> ~ ~
  -- L ~     ~ L
  (ignoreL 0 . ignoreL 1 . ignoreL 3 -> [b|00 11 00 00|]) -> (0,1,3,2)
  -- ~ ~ --> ~ ~
  -- ~ L     L ~
  (ignoreL 0 . ignoreL 1 . ignoreL 2 -> [b|11 00 00 00|]) -> (0,1,3,2)

  _ -> case (wenv .&. [b|01010101|]) of
    -- * ~ --> ~ ~
    -- ~ ~     * ~
    [b|00 00 00 01|] -> (2,1,0,3)
    -- * * --> * ~
    -- * ~     * *
    [b|00 01 01 01|] -> (0,3,2,1)
    -- * * --> ~ ~
    -- ~ ~     * *
    [b|00 00 01 01|] -> (2,3,0,1)
    -- ~ * --> ~ ~
    -- * ~     * *
    [b|00 01 01 00|] -> (0,3,2,1)
    -- ~ * --> ~ ~
    -- ~ ~     ~ *
    [b|00 00 01 00|] -> (0,3,2,1)
    -- * * --> ~ *
    -- ~ *     * *
    [b|01 00 01 01|] -> (2,1,0,3)
    -- * ~ --> ~ ~
    -- ~ *     * *
    [b|01 00 00 01|] -> (2,1,0,3)
    -- * ~ --> ~ ~
    -- * ~     * *
    [b|00 01 00 01|] -> (3,1,2,0)
    -- ~ * --> ~ ~
    -- ~ *     * *
    [b|01 00 01 00|] -> (0,2,1,3)
    _ -> (0,1,2,3)

