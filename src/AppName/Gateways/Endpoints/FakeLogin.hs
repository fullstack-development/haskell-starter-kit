{-# LANGUAGE DataKinds #-}

module AppName.Gateways.Endpoints.FakeLogin
  ( LoginResponse,
    LoginAPIDetached,
    fakeLoginEndpoint,
  )
where

import AppName.Auth.User (AuthenticatedUser (..))
import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Crypto.JOSE (Error)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Ext.Data.Aeson as J
import GHC.Generics (Generic)
import Servant (Handler, JSON, Post, ReqBody, err401, throwError, (:>))
import qualified Servant.Auth.Server as SAS

data LoginData = LoginData
  { ldUserId :: Int,
    ldPassword :: String
  }
  deriving (Eq, Show, Read, Generic)

instance J.ToJSON LoginData where
  toJSON = J.droppedPrefixDecode

instance J.FromJSON LoginData where
  parseJSON = J.droppedPrefixParse

data LoginResponse
  = LoginResponseSuccess BL.ByteString
  | LoginResponseError Error
  deriving (Eq, Show, Generic)

type LoginAPIDetached =
  "auth" :> "fake-login" :> ReqBody '[JSON] LoginData :> Post '[JSON] LoginResponse

instance J.ToJSON LoginResponse where
  toJSON (LoginResponseSuccess token) =
    J.object ["success" .= True, "result" .= show token]
  toJSON (LoginResponseError err) =
    J.object
      [ "success" .= False,
        "error"
          .= J.object
            [ "code" .= ("errorWhileBuildingToken" :: String),
              "message" .= ("Error occurred: " <> show err)
            ]
      ]

fakeLoginEndpoint :: (MonadIO m, MonadThrow m) => SAS.JWTSettings -> LoginData -> m LoginResponse
fakeLoginEndpoint jwtSettings (LoginData userId "Open Sesame") = do
  token <-
    liftIO $
      SAS.makeJWT
        (AuthenticatedClient userId)
        jwtSettings
        Nothing
  either (pure . LoginResponseError) (pure . LoginResponseSuccess) token
fakeLoginEndpoint _ _ = throw err401
