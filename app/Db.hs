module Db (getRestaurantsFromDb) where

import           Data.Time
import           Restaurant

restaurants :: [Restaurant]
restaurants =
  [ Restaurant "Las Amigas" [] []
  , Restaurant "Oriental Delight" [Monday] []
  , Restaurant "Jim's Poke" [] ["alice"]
  , Restaurant "Random Steakhouse" [] ["bob"]
  ]

getRestaurantsFromDb :: IO [Restaurant]
getRestaurantsFromDb = pure restaurants
