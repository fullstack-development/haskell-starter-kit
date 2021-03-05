{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module AppName.API
  ( API,
    apiType,
  )
where

import AppName.Auth (AuthenticatedUser, ProtectedWithJWT)
import qualified AppName.Config as C
import AppName.Gateways.Endpoints.FakeLogin (LoginAPIDetached)
import AppName.Serializers.User (UserSerializer)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Servant (Capture, Get, JSON, (:<|>) (..), (:>))
import qualified Servant.Auth as SAS

data TestResponse = TestResponse
  { responseStatus :: Bool,
    responseText :: T.Text
  }

type API =
  LoginAPIDetached :<|> GetUsersAPI

type GetUsersAPI = "get-user-by-id" :> Capture "userId" Int :> Get '[JSON] (Maybe UserSerializer) :<|> ProtectedWithJWT :> "get-me" :> Get '[JSON] (Maybe UserSerializer)

apiType :: Proxy API
apiType = Proxy
