{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Pushbullet.API where

import           Control.Monad.Trans.Either    (EitherT)
import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text)
import qualified Pushbullet.Models.Push        as Push
import qualified Pushbullet.Models.PushRequest as PushRequest
import           Servant.API
import           Servant.Client

type V2 = "pushes" :> ReqBody '[JSON] PushRequest.PushRequest
                   :> Post '[JSON] Push.Push


newtype AccessToken = AccessToken Text

instance ToText AccessToken where
  toText (AccessToken x) = x

instance FromText AccessToken where
  fromText x = Just $ AccessToken x


type API = Header "Access-Token" AccessToken :> "v2" :> V2

api :: Proxy API
api = Proxy

createPush :: Maybe AccessToken -> PushRequest.PushRequest -> EitherT ServantError IO Push.Push
createPush = client api (BaseUrl Https "api.pushbullet.com" 443)
