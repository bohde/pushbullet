{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pushbullet.Models.PushRequest (
  Target(..),
  Message(..),
  PushRequest(..)
  ) where

import           Control.Applicative    ((<$>), (<*>), (<|>))
import           Data.Aeson             (FromJSON, ToJSON, object, parseJSON,
                                         toJSON, (.:), (.:?), (.=))
import qualified Data.Aeson.Types       as A
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           Pushbullet.Models.Push (PushType)

data Target = All
            | Device T.Text
            | Email T.Text
            | Channel T.Text
            | Client T.Text
            deriving (Show, Eq, Generic)


targetPairs :: Target -> [A.Pair]
targetPairs All = []
targetPairs (Device t) = ["device_id" .= t]
targetPairs (Email t) = ["email" .= t]
targetPairs (Channel t) = ["channel_tag" .= t]
targetPairs (Client t) = ["client_iden" .= t]

instance ToJSON Target where
  toJSON = object . targetPairs

instance FromJSON Target where
  parseJSON (A.Object v) =     (Device <?> "device_id")
                           <|> (Email <?> "email")
                           <|> (Channel <?> "channel_tag")
                           <|> (Client <?> "client_iden")
                           <|> return All
    where
      f <?> k = f <$> v .: k
  parseJSON invalid = A.typeMismatch "Target" invalid



data Message = Note (Maybe T.Text) (Maybe T.Text)
             | Link (Maybe T.Text) (Maybe T.Text) (Maybe T.Text)
             deriving (Show, Eq, Generic)

messagePairs :: Message -> [A.Pair]
messagePairs (Note title body) = [
    "type" .= ("note" :: T.Text),
    "title" .= title,
    "body" .= body
    ]
messagePairs (Link title body url) = [
    "type" .= ("link" :: T.Text),
    "title" .= title,
    "body" .= body,
    "url" .= url
    ]

instance ToJSON Message where
  toJSON = object . messagePairs

instance FromJSON Message where
  parseJSON (A.Object v) = do
    (t :: T.Text) <- v .: "type"
    case t of
      "note" -> Note <$> v .:? "title" <*> v .:? "body"
      "link" -> Link <$> v .:? "title" <*> v .:? "body" <*> v .:? "url"
      _ -> fail "type should be one of \"note\" or \"link\"."
  parseJSON invalid = A.typeMismatch "Target" invalid


data PushRequest = PushRequest Target Message deriving (Show, Eq, Generic)

instance ToJSON PushRequest where
  toJSON (PushRequest t m) = object $ targetPairs t <> messagePairs m

instance FromJSON PushRequest where
  parseJSON v = PushRequest <$> parseJSON v <*> parseJSON v
