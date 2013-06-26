module Accelerate.World
       ( isFluid, isWall, isFire
       , weight, age
       , res
       , module Common.World)
where

import Data.Array.Accelerate (Acc, Array, Exp, DIM1, Z(..), (:.)(..), (?), (>=*), (<=*), (<*), (&&*), (||*), (==*))
import qualified Data.Array.Accelerate as A

import Common.World hiding (isFluid, isWall, isFire, weight, age)
import qualified Common.World as C
import Data.Word

res = A.index2 (A.lift resY) (A.lift resX)

-- Elements and properties -----------------------------------------------------

{-# INLINE isWall #-}
isWall :: Exp Element -> Exp Bool
isWall x = (x >=* 23 &&* x <=* 26) ||* x ==* 127

{-# INLINE isFire #-}
isFire :: Exp Element -> Exp Bool
isFire x = x >=* A.lift fire &&* x <=* A.lift fire_end

fluidArray :: Acc (Array DIM1 Word8)
fluidArray = A.use $ A.fromList (Z:. fromIntegral wall + 1) $ map C.isFluid [0..wall]

isFluid :: Exp Element -> Exp Weight
isFluid x = fluidArray A.!! A.fromIntegral x 


weightArray :: Acc (Array DIM1 Weight)
weightArray = A.use $ A.fromList (Z:. fromIntegral wall + 1) $ map C.weight [0..wall]

{-# INLINE weight #-}
weight :: Exp Element -> Exp Weight
weight x = weightArray A.!! A.fromIntegral x 


ageArray :: Acc (Array DIM1 (Int, Element, Element))
ageArray = A.use $ A.fromList (Z :. fromIntegral wall + 1) $ map age' [0..wall]

{-# INLINE age #-}
age :: Exp Int -> Exp Element -> Exp Element
age r x = let (i,t,e) = A.unlift $ ageArray A.!! A.fromIntegral x 
           in (r <* i) ? (t,e)
