{-# LANGUAGE TemplateHaskell #-}
module Pushbullet.Models.Push (
  Push(..),
  Pushes(..),
  PushType(..),
  PushRequest(..),
  PushUpdate(..),
  Direction(..),
  User(..)
  ) where

import           Data.Aeson.Casing (aesonDrop, snakeCase)
import           Data.Aeson.TH     (constructorTagModifier, defaultOptions,
                                    deriveJSON)
import           Data.Char         (toLower)
import qualified Data.Text         as T
import           Data.Vector       (Vector)

data PushType = Note | File | Link deriving (Eq, Show)
$(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''PushType)

data Direction = Self | Outgoing | Incoming deriving (Show, Eq)
$(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''Direction)

data Push = Push {
  pushIden                    :: T.Text,
  pushActive                  :: Bool,
  pushCreated                 :: Float,
  pushModified                :: Float,
  pushType                    :: PushType,
  pushDismissed               :: Bool,
  pushGuid                    :: Maybe T.Text,
  pushDirection               :: Direction,
  pushSenderIden              :: T.Text,
  pushSenderEmail             :: T.Text,
  pushSenderEmailNormalized   :: T.Text,
  pushSenderName              :: T.Text,
  pushReceiverIden            :: T.Text,
  pushReceiverEmail           :: T.Text,
  pushReceiverEmailNormalized :: T.Text,
  pushTargetDeviceIden        :: Maybe T.Text,
  pushSourceDeviceIden        :: Maybe T.Text,
  pushClientIden              :: Maybe T.Text,
  pushChannelIden             :: Maybe T.Text,
  pushAwakeAppGuids           :: Maybe (Vector T.Text),
  pushTitle                   :: Maybe T.Text,
  pushBody                    :: Maybe T.Text,
  pushUrl                     :: Maybe T.Text,
  pushFileName                :: Maybe T.Text,
  pushFileType                :: Maybe T.Text,
  pushFileUrl                 :: Maybe T.Text,
  pushImageUrl                :: Maybe T.Text,
  pushImageWidth              :: Maybe Int,
  pushImageHeight             :: Maybe Int
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 4 snakeCase) ''Push)

data PushRequest = PushRequest {
  pushRequestType             :: PushType,
  pushRequestTitle            :: Maybe T.Text,
  pushRequestBody             :: Maybe T.Text,
  pushRequestUrl              :: Maybe T.Text,
  pushRequestFileName         :: Maybe T.Text,
  pushRequestFileType         :: Maybe T.Text,
  pushRequestFileUrl          :: Maybe T.Text,
  pushRequestSourceDeviceIden :: Maybe T.Text,
  pushRequestDeviceIden       :: Maybe T.Text,
  pushRequestClientIden       :: Maybe T.Text,
  pushRequestChannelTag       :: Maybe T.Text,
  pushRequestEmail            :: Maybe T.Text,
  pushRequestGuid             :: Maybe T.Text
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 11 snakeCase) ''PushRequest)

data Pushes = Pushes {
  pushes :: Vector Push
  } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Pushes)

data PushUpdate = PushUpdate {
  dismissed :: Bool
  } deriving (Show, Eq)

$(deriveJSON defaultOptions ''PushUpdate)


data User = User {
  userIden            :: T.Text,
  userCreated         :: Float,
  userModified        :: Float,
  userEmail           :: T.Text,
  userEmailNormalized :: T.Text,
  userName            :: T.Text,
  userImageUrl        :: T.Text,
  userMaxUploadSize   :: Int,
  userReferredCount   :: Maybe Int,
  userReferrerIden    :: Maybe T.Text
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 4 snakeCase) ''User)
