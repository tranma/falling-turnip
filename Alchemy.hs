module Alchemy where

import World

applyAlchemy :: Int -> Element -> Element -> (Element, Element)
-- water + salt = salt_water + nothing
applyAlchemy _ 7 10 = (salt_water, nothing)
applyAlchemy _ 10 7 = (nothing, salt_water)
applyAlchemy _ w 1 | isWall w = (wall, steam_cndns)
applyAlchemy _ 1 w | isWall w = (steam_cndns, wall)
applyAlchemy _ 7 f | isFire f = (steam_water, nothing)
applyAlchemy _ f 7 | isFire f = (nothing, steam_water) 
applyAlchemy _ f 8 | isFire f = (steam_water, salt)
applyAlchemy _ 8 f | isFire f = (steam_water, salt)
applyAlchemy _ 6 f | isFire f = (fire, fire)
applyAlchemy _ f 6 | isFire f = (fire, fire)
applyAlchemy _ 0 23 = (fire, torch)
applyAlchemy _ 23 0 = (torch, fire)
applyAlchemy r f 24 | isFire f = if r < 20 then (sand, fire) else (fire, fire)
applyAlchemy r 24 f | isFire f = if r < 20 then (fire, sand) else (fire, fire)
applyAlchemy _ 7 24 = (plant, plant)
applyAlchemy _ 24 7 = (plant, plant)
applyAlchemy _ 25 0 = (spout, water)
applyAlchemy _ 0 25 = (water, spout)
applyAlchemy _ a b = (a, b)