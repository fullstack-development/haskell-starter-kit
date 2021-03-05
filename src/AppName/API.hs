{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module AppName.API
  ( API,
    apiType,
  )
where

import qualified AppName.Config as C
import AppName.Serializers.User (UserSerializer)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Servant (Capture, Get, JSON, (:<|>) (..), (:>))

data TestResponse = TestResponse
  { responseStatus :: Bool,
    responseText :: T.Text
  }

type API =
  "test-endpoint-with" :> Capture "echotext" T.Text :> Get '[JSON] T.Text :<|> "get-user-by-id" :> Capture "userId" Int :> Get '[JSON] (Maybe UserSerializer)

apiType :: Proxy API
apiType = Proxy
