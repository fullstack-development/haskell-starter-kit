{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module AppName.API
  ( API,
    apiType,
  )
where

import qualified AppName.API.PhoneVerification as Phone
import AppName.API.User (PersonalInfoSerializer, UserSerializer)
import AppName.Auth (AuthenticatedUser, ProtectedWithJWT)
import qualified AppName.Config as C
import AppName.Gateways.Endpoints.FakeLogin (LoginData, LoginResponse)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Ext.HTTP.Response as Web
import Servant (Capture, Get, JSON, Post, ReqBody, (:<|>) (..), (:>))
import qualified Servant.Auth as SAS

data TestResponse = TestResponse
  { responseStatus :: Bool,
    responseText :: T.Text
  }

type API =
  LoginAPIDetached :<|> Phone.PhoneAuthAPI :<|> GetUsersAPI :<|> SaveUsersAPI

type LoginAPIDetached =
  "auth" :> "fake-login" :> ReqBody '[JSON] LoginData :> Post '[JSON] LoginResponse

type GetUsersAPI = "get-user-by-id" :> Capture "userId" Int :> Get '[JSON] (Maybe UserSerializer) :<|> ProtectedWithJWT :> "get-me" :> Get '[JSON] (Maybe UserSerializer)

type SaveUsersAPI = ProtectedWithJWT :> "users" :> "save-personal-info" :> ReqBody '[JSON] PersonalInfoSerializer :> Post '[JSON] (Web.WebApiHttpResponse ())

apiType :: Proxy API
apiType = Proxy
