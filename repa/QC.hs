module QC where

import Test.QuickCheck

import Step
import World
import Data.List
import Control.Applicative


elems = [nothing, steam_water] ++ [fire .. fire_end] ++ [oil, water, salt_water, sand, salt, stone, wall ]

environments = combine <$> ( (,,,) <$> elements elems <*> elements elems <*> elements elems <*> elements elems )

conservation :: Property
conservation 
  = forAll environments $ \env -> sort [weigh (env, 0), weigh (env, 1), weigh (env, 2), weigh (env, 3)] == [0,1,2,3]