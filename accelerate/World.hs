module Accelerate.World
       ( isFluid, isWall, isFire
       , weight, age
       , res
       , module Common.World)
where

import Data.Array.Accelerate (Acc, Array, Exp, DIM1, Z(..), (:.)(..), (?), lift, (>=*), (<=*), (<*), (&&*), (||*), (==*))
import qualified Data.Array.Accelerate as A

import Common.World hiding (isFluid, isWall, isFire, weight, age)

res = A.index2 (A.lift resY) (A.lift resX)

-- Elements and properties -----------------------------------------------------

elemsAcc :: Acc (Array DIM1 Element)
elemsAcc = A.use $ A.fromList (Z:.(length elems)) elems

{-# INLINE isWall #-}
isWall :: Exp Element -> Exp Bool
isWall x = (x >=* 23 &&* x <=* 26) ||* x ==* 127

{-# INLINE isFire #-}
isFire :: Exp Element -> Exp Bool
isFire x = x >=* A.lift fire &&* x <=* A.lift fire_end

{-# INLINE isFluid #-}
isFluid :: Exp Element -> Exp Weight
isFluid x = (x ==* 1 ||* x ==* 2 ||* x ==* 27 ||* (x >=* 6 &&* x <=* 8)) ? (2, 0)

{-# INLINE weight #-}
weight :: Exp Element -> Exp Weight
weight x = (x ==* 1 ||* x ==* 2) ? (0, (x ==* 0 ? (2, (x ==* 9) ? (lift' $ fromIntegral salt, (x ==* 27) ? (lift' $ fromIntegral water, isFire x ? (0, lift' $ fromIntegral x))))))
  where lift' :: Weight -> Exp Weight
        lift' = lift

{-# INLINE age #-}
age :: Exp Int -> Exp Element -> Exp Element
age r x = x ==* (lift fire_end) ? (lift nothing, isFire x ? (r <* 50 ? (x+1,x), x ==* (lift steam_water) ? (r <* 1 ? (lift water, lift steam_water), x ==* (lift steam_condensed) ? (r <* 5 ? (lift water, lift steam_condensed), x ==* (lift turnip) ? (elemsAcc A.! (A.index1 ((r * (lift $ length elems)) `div` 110)), x)))))
