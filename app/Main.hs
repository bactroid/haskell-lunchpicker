{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AWSLambda
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.HashMap.Strict        as M
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Db
import           GHC.Generics
import           Picker
import           Restaurant
import           Slack
import           System.Environment         (getEnvironment)

data RespBody = RespBody
  { repsonse_type :: String
  , text          :: String
  } deriving (Generic, Eq, Show, Aeson.ToJSON, Aeson.FromJSON)

data HttpResponse = HttpResponse
  { statusCode :: Int
  , body       :: String
  } deriving (Generic, Eq, Show, Aeson.ToJSON, Aeson.FromJSON)

errorMsg :: String
errorMsg = "I'm having trouble. Maybe just go for coffee?"

getTableName :: IO (Maybe String)
getTableName = do
  tableVal <- filter ((== "LUNCH_TABLE") . fst) <$> getEnvironment
  if null tableVal
    then return Nothing
    else return (Just . snd . head $ tableVal)

stringify :: Aeson.ToJSON a => a -> String
stringify = BC.unpack . Aeson.encode

getBodyText :: Aeson.Value -> Maybe T.Text
getBodyText (Aeson.Object x) = case M.lookup "body" x of
                                 Just (Aeson.String s) -> Just s
                                 Nothing               -> Nothing
                                 _                     -> Nothing
getBodyText _ = Nothing

eventToUserList :: Aeson.Value -> [User]
eventToUserList e = fromMaybe [] (getBodyText e >>= parseUserList)

selectValidRestaurant :: Aeson.Value -> [Restaurant] -> IO Restaurant
selectValidRestaurant e = getRandomRestaurant . removeVetoedChoices (eventToUserList e)

main :: IO ()
main = lambdaMain handler

handler :: Aeson.Value -> IO HttpResponse
handler evt = do
  tblVal <- getTableName
  case tblVal of
    (Just tbl) -> do
      restaurants <- getRestaurantsFromDb tbl
      restaurant <- selectValidRestaurant evt restaurants
      return (HttpResponse 200 (stringify (RespBody "in_channel" (name restaurant))))
    Nothing ->
      return (HttpResponse 200 (stringify (RespBody "in_channel" errorMsg)))
