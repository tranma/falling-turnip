{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
module Accelerate.Gravity
      (applyGravity)
where

import Data.Array.Accelerate as A
import Prelude as P
import Common.World
import Repa.Gravity(applyGravity')

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

{-# INLINE applyGravity #-}
applyGravity :: Exp WeightEnv -> Exp MargPos -> Exp MargPos
applyGravity x n = let (a,b,c,d) = A.unlift $ gravityArray A.!! (A.fromIntegral x)
                    in (n ==* 0) ? (a , (n ==* 1) ? (b, (n ==* 2) ? (c,d)))

gravityArray :: Acc (Array DIM1 (MargPos, MargPos, MargPos, MargPos))
gravityArray = A.use $ fromList (Z :. 255) $ P.map applyGravity' [0..255]

