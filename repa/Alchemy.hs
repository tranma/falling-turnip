module Repa.Alchemy where

import Common.World

{-# INLINE applyAlchemy #-}
applyAlchemy :: Int -> Element -> Element -> (Element, Element)
-- water + salt = salt_water + nothing
applyAlchemy _ 7 10 = (salt_water, nothing)
applyAlchemy _ 10 7 = (nothing, salt_water)

-- steam condenses: <some wall> + steam = <wall> + condensed steam
applyAlchemy _ w 1 | isWall w = (wall, steam_condensed)
applyAlchemy _ 1 w | isWall w = (steam_condensed, wall)

-- water evaporates: water/salt_water + <some fire> = steam + nothing
applyAlchemy _ 7 f | isFire f = (steam_water, nothing)
applyAlchemy _ f 7 | isFire f = (nothing, steam_water)
applyAlchemy _ f 8 | isFire f = (steam_water, salt)
applyAlchemy _ 8 f | isFire f = (steam_water, salt)

-- oil catches fire: oil + <some fire> = 2 x new fire
applyAlchemy _ 6 f | isFire f = (fire, fire)
applyAlchemy _ f 6 | isFire f = (fire, fire)

-- torch generates fire: torch + nothing = torch + fire
applyAlchemy _ 0 23 = (fire, torch)
applyAlchemy _ 23 0 = (torch, fire)

-- spout generates water: spout + nothing = spout + water
applyAlchemy _ 25 0 = (spout, water)
applyAlchemy _ 0 25 = (water, spout)

-- fire burns plant: <some fire> + plant = new fire + sand
applyAlchemy r f 24 | isFire f = if r < 20 then (sand, fire) else (fire, fire)
applyAlchemy r 24 f | isFire f = if r < 20 then (fire, sand) else (fire, fire)

-- water grows plant: water + plant = 2 x plant
applyAlchemy _ 7 24 = (plant, plant)
applyAlchemy _ 24 7 = (plant, plant)

-- water eroses metal: water/salt_water + metal = water/salt_water + sand
applyAlchemy r 26 7 = if r < 1 then (sand, water) else (metal, water)
applyAlchemy r 7 26 = if r < 1 then (water, sand) else (water, metal)
applyAlchemy r 26 8 = if r < 3 then (sand, salt_water) else (metal, salt_water)
applyAlchemy r 8 26 = if r < 3 then (salt_water, sand) else (salt_water, metal)

-- lava + stone = 2 x lava
applyAlchemy r 27 11 = if r < 5 then (lava, lava) else (lava, stone)
applyAlchemy r 11 27 = if r < 5 then (lava, lava) else (stone, lava)

-- lava + metal/sand/salt = 2 x lava
applyAlchemy r 27 26 = if r < 1 then (lava, lava) else (lava, metal)
applyAlchemy r 26 27 = if r < 1 then (lava, lava) else (metal, lava)
applyAlchemy r 27 9  = if r < 50 then (lava, lava) else (lava, sand)
applyAlchemy r 9 27  = if r < 50 then (lava, lava) else (sand, lava)
applyAlchemy r 27 10 = if r < 50 then (lava, lava) else (lava, salt)
applyAlchemy r 10 27 = if r < 50 then (lava, lava) else (salt, lava)

-- lava + oil/plant = lava + fire
applyAlchemy r 27 6 = if r < 80 then (lava, fire) else (lava, oil)
applyAlchemy r 6 27 = if r < 80 then (fire, lava) else (oil, lava)
applyAlchemy r 27 24 = if r < 80 then (lava, fire) else (lava, plant)
applyAlchemy r 24 27 = if r < 80 then (fire, lava) else (plant, lava)

-- water + lava = steam + stone
applyAlchemy _ 7 27 = (steam_water, stone)
applyAlchemy _ 27 7 = (stone, steam_water)

-- salt_water + lava = steam + stone OR steam + salt
applyAlchemy r 8 27 = if r < 20 then (steam_water, salt) else (steam_water, stone)
applyAlchemy r 27 8 = if r < 20 then (salt, steam_water) else (stone, steam_water)


applyAlchemy _ a b = (a, b)


applyAlchemy' :: Element -> Element -> Either (Element, Element)
                                              (Int, (Element, Element), (Element, Element))
-- water + salt = salt_water + nothing
applyAlchemy' 7 10 = Left (salt_water, nothing)
applyAlchemy' 10 7 = Left (nothing, salt_water)

-- steam condenses: <some wall> + steam = <wall> + condensed steam
applyAlchemy' w 1 | isWall w = Left (wall, steam_condensed)
applyAlchemy' 1 w | isWall w = Left (steam_condensed, wall)

-- water evaporates: water/salt_water + <some fire> = steam + nothing
applyAlchemy' 7 f | isFire f = Left (steam_water, nothing)
applyAlchemy' f 7 | isFire f = Left (nothing, steam_water)
applyAlchemy' f 8 | isFire f = Left (steam_water, salt)
applyAlchemy' 8 f | isFire f = Left (steam_water, salt)

-- oil catches fire: oil + <some fire> = 2 x new fire
applyAlchemy' 6 f | isFire f = Left (fire, fire)
applyAlchemy' f 6 | isFire f = Left (fire, fire)

-- torch generates fire: torch + nothing = torch + fire
applyAlchemy' 0 23 = Left (fire, torch)
applyAlchemy' 23 0 = Left (torch, fire)

-- spout generates water: spout + nothing = spout + water
applyAlchemy' 25 0 = Left (spout, water)
applyAlchemy' 0 25 = Left (water, spout)

-- fire burns plant: <some fire> + plant = new fire + sand
applyAlchemy' f 24 | isFire f = Right (20 , (sand, fire) , (fire, fire))
applyAlchemy' 24 f | isFire f = Right (20 , (fire, sand) , (fire, fire))

-- water grows plant: water + plant = 2 x plant
applyAlchemy' 7 24 = Left (plant, plant)
applyAlchemy' 24 7 = Left (plant, plant)

-- water eroses metal: water/salt_water + metal = water/salt_water + sand
applyAlchemy' 26 7 = Right (1 , (sand, water) , (metal, water))
applyAlchemy' 7 26 = Right (1 , (water, sand) , (water, metal))
applyAlchemy' 26 8 = Right (3 , (sand, salt_water) , (metal, salt_water))
applyAlchemy' 8 26 = Right (3 , (salt_water, sand) , (salt_water, metal))

-- lava + stone = 2 x lava
applyAlchemy' 27 11 = Right (5 , (lava, lava) , (lava, stone))
applyAlchemy' 11 27 = Right (5 , (lava, lava) , (stone, lava))

-- lava + metal/sand/salt = 2 x lava
applyAlchemy' 27 26 = Right ( 1  , (lava, lava) , (lava, metal))
applyAlchemy' 26 27 = Right ( 1  , (lava, lava) , (metal, lava))
applyAlchemy' 27 9  = Right ( 50 , (lava, lava) , (lava, sand))
applyAlchemy' 9 27  = Right ( 50 , (lava, lava) , (sand, lava))
applyAlchemy' 27 10 = Right ( 50 , (lava, lava) , (lava, salt))
applyAlchemy' 10 27 = Right ( 50 , (lava, lava) , (salt, lava))

-- lava + oil/plant = lava + fire
applyAlchemy' 27 6  = Right ( 80 , (lava, fire) , (lava, oil))
applyAlchemy' 6 27  = Right ( 80 , (fire, lava) , (oil, lava))
applyAlchemy' 27 24 = Right ( 80 , (lava, fire) , (lava, plant))
applyAlchemy' 24 27 = Right ( 80 , (fire, lava) , (plant, lava))

-- water + lava = steam + stone
applyAlchemy' 7 27 = Left (steam_water, stone)
applyAlchemy' 27 7 = Left (stone, steam_water)

-- salt_water + lava = steam + stone OR steam + salt
applyAlchemy' 8 27 = Right (20, (steam_water, salt) , (steam_water, stone))
applyAlchemy' 27 8 = Right (20, (salt, steam_water) , (stone, steam_water))


applyAlchemy' a b = Left (a, b)


