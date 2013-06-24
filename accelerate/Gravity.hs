{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
module Accelerate.Gravity
      (applyGravity)
where

import Data.Bits
import Language.Literals.Binary
import Data.Array.Accelerate as A
import Prelude as P
import Common.World

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

mkExp
  :: (Elt a, Elt v)
  => (Exp v)                    -- input argument
  -> (Exp v -> Exp Bool)        -- predicate on input argument
  -> Exp a -> Exp a -> Exp a    -- if then else expression
mkExp v1 p1 = P.curry (p1 v1 ?)

{-# INLINE applyGravity #-}
applyGravity :: Exp WeightEnv -> Exp MargPos -> Exp MargPos
applyGravity x n = let (a,b,c,d) = unlift $ applyGravity' x
                    in (n ==* 0) ? (a , (n ==* 1) ? (b, (n ==* 2) ? (c,d)))

{-# INLINE ignoreL #-}
ignoreL :: Exp MargPos -> Exp WeightEnv -> Exp WeightEnv
ignoreL n x = x .&. (A.rotateL [b|11 11 11 01|] (n * 2))

{-# INLINE applyGravity' #-}
applyGravity' :: Exp WeightEnv -> Exp (MargPos, MargPos, MargPos, MargPos)
applyGravity' wenv = foldr (P.uncurry $ mkExp wenv) (foldr (P.uncurry $ mkExp (wenv .&. [b|01010101|])) (lift' (0,1,2,3)) elseList) thenList
  where (~>) a b = (a,b)
        thenList :: [(Exp WeightEnv -> Exp Bool, Exp (MargPos, MargPos, MargPos, MargPos))]
        elseList :: [(Exp WeightEnv -> Exp Bool, Exp (MargPos, MargPos, MargPos, MargPos))]
        lift' :: (MargPos,MargPos,MargPos,MargPos) -> Exp (MargPos,MargPos,MargPos,MargPos) 
        lift' = lift
        thenList =
                   -- L L --> L L
                   -- L ~     ~ L
                   [ ((==*[b|00 11 11 11|]) . ignoreL 3) ~> lift' (0,1,3,2)
                   -- L L -~> L L
                   -- ~ L     L ~
                   , ((==*[b|11 00 11 11|]) . ignoreL 2) ~> lift' (0,1,3,2)
                   -- L ~ -~> ~ L
                   -- * *     * *
                   , ((==*[b|01 01 00 11|]) . ignoreL 1 . ignoreL 2 . ignoreL 3) ~> lift' (1,0,2,3)
                   -- ~ L -~> L ~
                   -- * *     * *
                   , ((==*[b|01 01 11 00|]) . ignoreL 0 . ignoreL 2 . ignoreL 3) ~> lift' (1,0,2,3)
                   -- ~ ~ -~> ~ ~
                   -- L ~     ~ L
                   , ((==*[b|00 11 00 00|]) . ignoreL 0 . ignoreL 1 . ignoreL 3) ~> lift' (0,1,3,2)
                   -- ~ ~ -~> ~ ~
                   -- ~ L     L ~
                   , ((==*[b|11 00 00 00|]) . ignoreL 0 . ignoreL 1 . ignoreL 2) ~> lift' (0,1,3,2)
                   ]
        elseList =
                   -- * ~ -~> ~ ~
                   -- ~ ~     * ~
                   [ (==*[b|00 00 00 01|]) ~> lift' (2,1,0,3)
                   -- * * -~> * ~
                   -- * ~     * *
                   , (==*[b|00 01 01 01|]) ~> lift' (0,3,2,1)
                   -- * * -~> ~ ~
                   -- ~ ~     * *
                   , (==*[b|00 00 01 01|]) ~> lift' (2,3,0,1)
                   -- ~ * -~> ~ ~
                   -- * ~     * *
                   , (==*[b|00 01 01 00|]) ~> lift' (0,3,2,1)
                   -- ~ * -~> ~ ~
                   -- ~ ~     ~ *
                   , (==*[b|00 00 01 00|]) ~> lift' (0,3,2,1)
                   -- * * -~> ~ *
                   -- ~ *     * *
                   , (==*[b|01 00 01 01|]) ~> lift' (2,1,0,3)
                   -- * ~ -~> ~ ~
                   -- ~ *     * *
                   , (==*[b|01 00 00 01|]) ~> lift' (2,1,0,3)
                   -- * ~ -~> ~ ~
                   -- * ~     * *
                   , (==*[b|00 01 00 01|]) ~> lift' (3,1,2,0)
                   -- ~ * -~> ~ ~
                   -- ~ *     * *
                   , (==*[b|01 00 01 00|]) ~> lift' (0,2,1,3)
                   ]
