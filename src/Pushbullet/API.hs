{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Pushbullet.API where

import           Control.Monad.Trans.Either (EitherT)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import qualified Pushbullet.Models.Push     as Push
import           Servant.API
import           Servant.Client

type API = Header "Access-Token" AccessToken :> "v2" :> "pushes" :> Get '[JSON] Push.Pushes
      :<|> Header "Access-Token" AccessToken :> "v2" :> "pushes" :> ReqBody '[JSON] Push.PushRequest :> Post '[JSON] Push.Push
      :<|> Header "Access-Token" AccessToken :> "v2" :> "pushes" :> Capture "iden" Text :> ReqBody '[JSON] Push.PushUpdate :> Post '[JSON] Push.Push
      :<|> Header "Access-Token" AccessToken :> "v2" :> "pushes" :> Capture "iden" Text :> Delete '[JSON] ()

newtype AccessToken = AccessToken Text

instance ToText AccessToken where
  toText (AccessToken x) = x

instance FromText AccessToken where
  fromText x = Just $ AccessToken x


api :: Proxy API
api = Proxy

listPushes :: Maybe AccessToken -> EitherT ServantError IO Push.Pushes
createPush :: Maybe AccessToken -> Push.PushRequest -> EitherT ServantError IO Push.Push
updatePush :: Maybe AccessToken -> Text -> Push.PushUpdate -> EitherT ServantError IO Push.Push
deletePush :: Maybe AccessToken -> Text -> EitherT ServantError IO ()
(listPushes :<|> createPush :<|> updatePush :<|> deletePush) = client api (BaseUrl Https "api.pushbullet.com" 443)
