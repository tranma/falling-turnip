module Accelerate.Alchemy where

import Prelude as P
import Data.Array.Accelerate

import Accelerate.World

mkExp2
  :: (Elt a, Elt v)
  => (Exp v, Exp v)                         -- input elements
  -> (Exp v -> Exp Bool, Exp v -> Exp Bool) -- predicates on input elements
  -> Exp a -> Exp a -> Exp a                -- if then else expression
mkExp2 (v1,v2) (p1,p2) = P.curry ((p1 v1 &&* p2 v2) ?)

applyAlchemy :: Exp Int -> Exp Element -> Exp Element -> Exp (Element, Element)
applyAlchemy r x y = foldr (P.uncurry $ mkExp2 (x,y)) (lift (x,y))
        -- water + salt = salt_water + nothing
       [((==* 7),(==* 10)) ~> lift (salt_water, nothing)
       , ((==* 10),(==* 7)) ~> lift (nothing, salt_water)

       -- steam condenses: <some wall> + steam = <wall> + condensed steam
       , (isWall,(==* 1)) ~> lift (wall, steam_condensed)
       , ((==* 1),isWall) ~> lift (steam_condensed, wall)

       -- water evaporates: water/salt_water + <some fire> = steam + nothing
       , ((==* 7),isFire) ~> lift (steam_water, nothing)
       , (isFire,(==* 7)) ~> lift (nothing, steam_water)
       , (isFire,(==* 8)) ~> lift (steam_water, salt)
       , ((==* 8),isFire) ~> lift (steam_water, salt)

       -- oil catches fire: oil + <some fire> = (==* 2) x new fire
       , ((==* 6),isFire) ~> lift (fire, fire)
       , (isFire,(==* 6)) ~> lift (fire, fire)

       -- torch generates fire: torch + nothing = torch + fire
       , ((==* 0),(==* 23)) ~> lift (fire, torch)
       , ((==* 23),(==* 0)) ~> lift (torch, fire)

       -- spout generates water: spout + nothing = spout + water
       , ((==* 25),(==* 0)) ~> lift (spout, water)
       , ((==* 0),(==* 25)) ~> lift (water, spout)

       -- fire burns plant: <some fire> + plant = new fire + sand
       , (isFire,(==* 24)) ~> ( r <* 20  ? (lift (sand, fire) , lift (fire, fire)))
       , ((==* 24),isFire) ~> ( r <* 20  ? (lift (fire, sand) , lift (fire, fire)))

       -- water grows plant: water + plant = (==* 2) x plant
       , ((==* 7),(==* 24)) ~> lift (plant, plant)
       , ((==* 24),(==* 7)) ~> lift (plant, plant)

       -- water eroses metal: water/salt_water + metal = water/salt_water + sand
       , ((==* 26),(==* 7)) ~> ( r <* 1  ? (lift (sand, water) , lift (metal, water)))
       , ((==* 7),(==* 26)) ~> ( r <* 1  ? (lift (water, sand) , lift (water, metal)))
       , ((==* 26),(==* 8)) ~> ( r <* 3  ? (lift (sand, salt_water) , lift (metal, salt_water)))
       , ((==* 8),(==* 26)) ~> ( r <* 3  ? (lift (salt_water, sand) , lift (salt_water, metal)))

       -- lava + stone = (==* 2) x lava
       , ((==* 27),(==* 11)) ~> ( r <* 5 ? (lift (lava, lava) , lift (lava, stone)))
       , ((==* 11),(==* 27)) ~> ( r <* 5 ? (lift (lava, lava) , lift (stone, lava)))

       -- lava + metal/sand/salt = (==* 2) x lava
       , ((==* 27),(==* 26)) ~> ( r <* 1  ? (lift (lava, lava) , lift (lava, metal)))
       , ((==* 26),(==* 27)) ~> ( r <* 1  ? (lift (lava, lava) , lift (metal, lava)))
       , ((==* 27),(==* 9)) ~> ( r <* 50  ? (lift (lava, lava) , lift (lava, sand)) )
       , ((==* 9),(==* 27)) ~> ( r <* 50  ? (lift (lava, lava) , lift (sand, lava)) )
       , ((==* 27),(==* 10)) ~> ( r <* 50  ? (lift (lava, lava) ,lift  (lava, salt)))
       , ((==* 10),(==* 27)) ~> ( r <* 50  ? (lift (lava, lava) ,lift  (salt, lava)))

       -- lava + oil/plant = lava + fire
       , ((==* 27),(==* 6))  ~> ( r <* 80  ? (lift (lava, fire) , lift (lava, oil)))
       , ((==* 6),(==* 27))  ~> ( r <* 80  ? (lift (fire, lava) , lift (oil, lava)))
       , ((==* 27),(==* 24)) ~> ( r <* 80  ? (lift (lava, fire) , lift (lava, plant)))
       , ((==* 24),(==* 27)) ~> ( r <* 80  ? (lift (fire, lava) , lift (plant, lava)))

       -- water + lava = steam + stone
       , ((==* 7),(==* 27)) ~> lift (steam_water, stone)
       , ((==* 27),(==* 7)) ~> lift (stone, steam_water)

       -- salt_water + lava = steam + stone OR steam + salt
       , ((==* 8),(==* 27)) ~> ( r <* 20 ? (lift (steam_water, salt) , lift (steam_water, stone)))
       , ((==* 27),(==* 8)) ~> ( r <* 20 ? (lift (salt, steam_water) , lift (stone, steam_water)))
       ]
  where (~>) :: a -> b -> (a,b)
        (~>) = (,)



