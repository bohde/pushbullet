{-# LANGUAGE TemplateHaskell #-}
module Pushbullet.Models.PushRequest (
  PushRequest(..)
  ) where

import           Data.Aeson.TH          (defaultOptions, deriveJSON,
                                         fieldLabelModifier)
import qualified Data.Text              as T
import           Pushbullet.Models.Push (PushType)

data PushRequest = PushRequest {
  device_iden :: Maybe T.Text,
  email       :: Maybe T.Text,
  channel_tag :: Maybe T.Text,
  client_iden :: Maybe T.Text,
  push_type   :: PushType,
  title       :: T.Text,
  body        :: T.Text
  } deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "push_type" then "type" else n)} ''PushRequest)
