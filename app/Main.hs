{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AWSLambda
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BC
-- import qualified Data.ByteString.Lazy.Internal as BS
import           Db
import           GHC.Generics
import           Picker
import           Restaurant

data RespBody = RespBody
  { repsonse_type :: String
  , text          :: String
  } deriving (Generic, Eq, Show, Aeson.ToJSON, Aeson.FromJSON)

data HttpResponse = HttpResponse
  { statusCode :: Int
  , body       :: String
  } deriving (Generic, Eq, Show, Aeson.ToJSON, Aeson.FromJSON)

stringify :: Aeson.ToJSON a => a -> String
stringify = BC.unpack . Aeson.encode

main :: IO ()
main = lambdaMain handler

handler :: Aeson.Value -> IO HttpResponse
handler evt = do
  putStrLn "This should go to logs"
  print evt
  restaurants <- getRestaurantsFromDb
  restaurant <- getRandomRestaurant restaurants
  pure (HttpResponse 200 (stringify (RespBody "in_channel" (name restaurant))))
