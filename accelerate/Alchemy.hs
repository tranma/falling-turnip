module Accelerate.Alchemy where

import Prelude as P
import Data.Array.Accelerate as A
import Repa.Alchemy(applyAlchemy')
import Accelerate.World

mkExp2
  :: (Elt a, Elt v)
  => (Exp v, Exp v)                         -- input elements
  -> (Exp v -> Exp Bool, Exp v -> Exp Bool) -- predicates on input elements
  -> Exp a -> Exp a -> Exp a                -- if then else expression
mkExp2 (v1,v2) (p1,p2) = P.curry ((p1 v1 &&* p2 v2) ?)

alchemyTable :: Acc (Array DIM2 (Int, (Element, Element), (Element, Element)))
alchemyTable = use $ A.fromList (Z :. P.fromIntegral wall + 1 :. P.fromIntegral wall + 1) 
               [ case applyAlchemy' x y
                  of Left (a,b) -> (0,(a,b),(a,b))
                     Right x    -> x 
               | x <- [0..wall] 
               , y <- [0..wall]
               ]



applyAlchemy :: Exp Int -> Exp Element -> Exp Element -> Exp (Element, Element)
applyAlchemy r x y =  let (i, t, e) = unlift $ alchemyTable ! lift (Z :. A.fromIntegral x :. A.fromIntegral y)
                       in (r <* i) ? (t , e)



