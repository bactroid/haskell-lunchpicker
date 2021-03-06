module Picker where

import           Data.List     (intersect)
import           Data.Time
import           Restaurant
import qualified System.Random as SR

openOnDay :: DayOfWeek -> Restaurant -> Bool
openOnDay day restaurant =  day `notElem` closed restaurant

notVetoedByAttendees :: [User] -> Restaurant -> Bool
notVetoedByAttendees attendees = null . intersect attendees . veto

removeVetoedChoices :: [User] -> [Restaurant] -> [Restaurant]
removeVetoedChoices attendees = filter (notVetoedByAttendees attendees)

selectRandom :: [a] -> IO a
selectRandom xs = (xs !!) <$> randomIndex
  where
    maxIndex = length xs - 1
    randomIndex = SR.getStdRandom (SR.randomR (0, maxIndex))

getToday :: IO DayOfWeek
getToday = (dayOfWeek . utctDay) <$> getCurrentTime

getRandomRestaurant :: [Restaurant] -> IO Restaurant
getRandomRestaurant choices = do
  today <- getToday
  if today == Friday
    then pure (Restaurant "Big Lou's" [] [])
    else selectRandom . filter (openOnDay today) $ choices
