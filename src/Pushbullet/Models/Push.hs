{-# LANGUAGE TemplateHaskell #-}
module Pushbullet.Models.Push (
  PushType(..),
  Direction(..),
  Push(..)
  ) where

import           Data.Aeson.TH (constructorTagModifier, defaultOptions,
                                deriveJSON, fieldLabelModifier)
import           Data.Char     (toLower)
import qualified Data.Text     as T
import           Data.Vector   (Vector)

data PushType = Note | File | Link deriving (Eq, Show)
$(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''PushType)

data Direction = Self | Outgoing | Incoming deriving (Show, Eq)
$(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''Direction)

data Push = Push {
  iden                      :: T.Text,
  active                    :: Bool,
  created                   :: Float,
  modified                  :: Float,
  push_type                 :: PushType,
  dismissed                 :: Bool,
  guid                      :: Maybe T.Text,
  direction                 :: Direction,
  sender_iden               :: T.Text,
  sender_email              :: T.Text,
  sender_email_normalized   :: T.Text,
  sender_name               :: T.Text,
  receiver_iden             :: T.Text,
  receiver_email            :: T.Text,
  receiver_email_normalized :: T.Text,
  target_device_iden        :: Maybe T.Text,
  source_device_iden        :: Maybe T.Text,
  client_iden               :: Maybe T.Text,
  channel_iden              :: Maybe T.Text,
  awake_app_guids           :: Maybe (Vector T.Text),
  title                     :: T.Text,
  body                      :: T.Text,
  url                       :: Maybe T.Text,
  file_name                 :: Maybe T.Text,
  file_type                 :: Maybe T.Text,
  file_url                  :: Maybe T.Text,
  image_url                 :: Maybe T.Text,
  image_width               :: Maybe Int,
  image_height              :: Maybe Int
  } deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "push_type" then "type" else n)} ''Push)
