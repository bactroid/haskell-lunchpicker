{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AWSLambda
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BC
import           Db
import           GHC.Generics
import           Picker
import           Restaurant
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

main :: IO ()
main = lambdaMain handler

handler :: Aeson.Value -> IO HttpResponse
handler _ = do
  tblVal <- getTableName
  case tblVal of
    (Just tbl) -> do
      putStrLn tbl
      restaurants <- getRestaurantsFromDb tbl
      restaurant <- getRandomRestaurant restaurants
      return (HttpResponse 200 (stringify (RespBody "in_channel" (name restaurant))))
    Nothing ->
      return (HttpResponse 200 (stringify (RespBody "in_channel" errorMsg)))
