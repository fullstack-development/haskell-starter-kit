module AppName.Serializers.User
  ( UserSerializer (..),
  )
where

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Ext.Data.Aeson as J
import GHC.Generics (Generic)

data UserSerializer = UserSerializer
  { userId :: Int,
    userCreatedAt :: Time.UTCTime,
    userPhone :: T.Text
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON UserSerializer where
  toJSON = J.droppedPrefixDecode
