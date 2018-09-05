{-# LANGUAGE OverloadedStrings #-}

module Db where

import           Control.Lens
import           Control.Monad.Trans.AWS
import qualified Data.HashMap.Strict     as M
import           Data.Maybe              (catMaybes, fromJust, isNothing,
                                          mapMaybe)
import qualified Data.Text               as T
import           Data.Time
import           Network.AWS.DynamoDB
import           Restaurant

type DynamoRecord = M.HashMap T.Text AttributeValue

getRestaurantsFromDb :: IO [Restaurant]
getRestaurantsFromDb = do
  dbRecs <- getDynamoDbData
  return $ mapMaybe dynamoRecordToRestaurant dbRecs

getDynamoDbData :: IO [DynamoRecord]
getDynamoDbData = do
  env <- newEnv Discover
  resp <- runResourceT . runAWST env . within NorthVirginia $ send (scan "lunch-picker-dev-restaurants")
  return $ resp ^. srsItems

getName :: DynamoRecord -> Maybe String
getName x = T.unpack <$> (M.lookup "name" x >>= view avS)

getList :: T.Text -> DynamoRecord -> [String]
getList prop x = T.unpack <$> mapMaybe (view avS) (catMaybes (traverse (view avL) (M.lookup prop x)))

getVeto :: DynamoRecord -> [String]
getVeto = getList "veto"

getClosed :: DynamoRecord -> [DayOfWeek]
getClosed = mapMaybe stringToDayOfWeek . getList "closed"

stringToDayOfWeek :: String -> Maybe DayOfWeek
stringToDayOfWeek "Sunday"    = Just Sunday
stringToDayOfWeek "Monday"    = Just Monday
stringToDayOfWeek "Tuesday"   = Just Tuesday
stringToDayOfWeek "Wednesday" = Just Wednesday
stringToDayOfWeek "Thursday"  = Just Thursday
stringToDayOfWeek "Friday"    = Just Friday
stringToDayOfWeek "Saturday"  = Just Saturday
stringToDayOfWeek _           = Nothing

dynamoRecordToRestaurant :: DynamoRecord -> Maybe Restaurant
dynamoRecordToRestaurant x = if isNothing dbName
                             then Nothing
                             else Just (Restaurant (fromJust dbName) dbClosed dbVeto)
  where
    dbName = getName x
    dbClosed = getClosed x
    dbVeto = getVeto x
