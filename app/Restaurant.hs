module Restaurant where

import           Data.Time

type User = String

data Restaurant = Restaurant
  { name   :: String
  , closed :: [DayOfWeek]
  , veto   :: [User]
  } deriving (Eq, Show)
