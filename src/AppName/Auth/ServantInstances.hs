{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AppName.Auth.ServantInstances where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Servant ((:>))
import qualified Servant.Auth.Server as SAS
import Servant.Foreign
  ( Arg (..),
    Foreign (..),
    HasForeign,
    HasForeignType,
    HeaderArg (..),
    PathSegment (..),
    typeFor,
    _reqHeaders,
  )

instance
  forall a lang ftype api.
  ( HasForeign lang ftype api,
    HasForeignType lang ftype T.Text
  ) =>
  HasForeign lang ftype (SAS.Auth '[SAS.JWT] a :> api)
  where
  type Foreign ftype (SAS.Auth '[SAS.JWT] a :> api) = Foreign ftype api
  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) req
    where
      req = subR {_reqHeaders = HeaderArg arg : _reqHeaders subR}
      arg =
        Arg
          { _argName = PathSegment "Authorization",
            _argType =
              typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy T.Text)
          }
