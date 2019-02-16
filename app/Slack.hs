{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Slack (parseUserList) where

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text                  as T
import           GHC.Generics
import           Restaurant
import           Web.FormUrlEncoded

data SlackFormData = SlackFormData
  { token         :: String
  , team_id       :: String
  , team_domain   :: String
  , enterprise_id :: Maybe String
  , channel_id    :: String
  , channel_name  :: String
  , user_name     :: String
  , command       :: String
  , text          :: String
  , response_url  :: String
  , trigger_id    :: String
  } deriving (Generic, Eq, Show, FromForm)


trim :: String -> String
trim = T.unpack . T.strip . T.pack

parseAttendeeList :: String -> [User]
parseAttendeeList = map (drop 1) . filter (\s -> head s == '@') . words . trim

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _)  = Nothing

textToByteString :: T.Text -> BC.ByteString
textToByteString = BC.pack . T.unpack

parseUserList :: T.Text -> Maybe [User]
parseUserList x = fmap (parseAttendeeList . text) ((eitherToMaybe . urlDecodeAsForm . textToByteString) x)
