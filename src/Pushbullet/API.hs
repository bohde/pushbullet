{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Pushbullet.API (
  listPushes,
  createPush,
  updatePush,
  deletePush
  ) where

import           Control.Monad.Trans.Either (EitherT)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import qualified Pushbullet.API.Models      as M
import           Servant.API
import           Servant.Client

type API = Header "Access-Token" AccessToken :> "v2" :> "pushes" :> Get '[JSON] M.Pushes
      :<|> Header "Access-Token" AccessToken :> "v2" :> "pushes" :> ReqBody '[JSON] M.PushRequest :> Post '[JSON] M.Push
      :<|> Header "Access-Token" AccessToken :> "v2" :> "pushes" :> Capture "iden" Text :> ReqBody '[JSON] M.PushUpdate :> Post '[JSON] M.Push
      :<|> Header "Access-Token" AccessToken :> "v2" :> "pushes" :> Capture "iden" Text :> Delete '[JSON] ()

newtype AccessToken = AccessToken Text

instance ToText AccessToken where
  toText (AccessToken x) = x

instance FromText AccessToken where
  fromText x = Just $ AccessToken x


api :: Proxy API
api = Proxy

listPushes :: Maybe AccessToken -> EitherT ServantError IO M.Pushes
createPush :: Maybe AccessToken -> M.PushRequest -> EitherT ServantError IO M.Push
updatePush :: Maybe AccessToken -> Text -> M.PushUpdate -> EitherT ServantError IO M.Push
deletePush :: Maybe AccessToken -> Text -> EitherT ServantError IO ()
(listPushes :<|> createPush :<|> updatePush :<|> deletePush) = client api (BaseUrl Https "api.pushbullet.com" 443)
