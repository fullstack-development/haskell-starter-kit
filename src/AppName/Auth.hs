{-# LANGUAGE DataKinds #-}

module AppName.Auth
  ( SAS.defaultJWTSettings,
    retrieveKey,
    module Export,
    ProtectedServantJWTCtx,
    ProtectedWithJWT,
    servantCtx,
    getAuthUser,
  )
where

import AppName.Auth.ServantInstances ()
import AppName.Auth.User as Export
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class
import Crypto.JOSE.JWK (JWK)
import Data.Proxy
import Servant
import qualified Servant.Auth.Server as SAS

retrieveKey :: FilePath -> IO JWK
retrieveKey = SAS.readKey

type ProtectedWithJWT = SAS.Auth '[SAS.JWT] AuthenticatedUser

type ProtectedServantJWTCtx = Proxy '[SAS.CookieSettings, SAS.JWTSettings]

servantCtx :: JWK -> Context '[SAS.CookieSettings, SAS.JWTSettings]
servantCtx authKey =
  let jwtSettings = SAS.defaultJWTSettings authKey
   in SAS.defaultCookieSettings :. jwtSettings :. EmptyContext

getAuthUser :: SAS.AuthResult AuthenticatedUser -> Maybe AuthenticatedUser
getAuthUser (SAS.Authenticated user) = Just user
getAuthUser _ = Nothing
