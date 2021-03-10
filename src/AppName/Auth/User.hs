{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module AppName.Auth.User
  ( AuthenticatedUser (..),
  )
where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Servant.Auth.Server as SAS

data AuthenticatedUser
  = AuthenticatedClient {auClientID :: Int}
  | AuthenticatedAdmin {auAdminID :: Int}
  deriving (Show, Generic, SAS.ToJWT, SAS.FromJWT)

instance J.ToJSON AuthenticatedUser where
  toJSON (AuthenticatedClient clientId) =
    J.object ["role" .= ("client" :: String), "clientId" .= clientId]
  toJSON (AuthenticatedAdmin adminId) =
    J.object ["role" .= ("admin" :: String), "adminId" .= adminId]

instance J.FromJSON AuthenticatedUser where
  parseJSON =
    J.withObject "AuthenticatedUser" $ \obj -> do
      role <- obj .: "role"
      case (role :: String) of
        "client" -> AuthenticatedClient <$> obj .: "clientId"
        "admin" -> AuthenticatedAdmin <$> obj .: "adminId"
        r -> fail $ "Can't parse AuthenticatedUser. Got " <> r

type instance
  SAS.BasicAuthCfg =
    SAS.BasicAuthData -> IO (SAS.AuthResult AuthenticatedUser)

instance SAS.FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData
