module Alchemy where

import World

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

applyAlchemy _ a b = (a, b)