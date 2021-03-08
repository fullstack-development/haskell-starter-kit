module AppName.API.User
  ( UserSerializer (..),
    AddressSerializer (..),
    PersonalInfoSerializer (..),
  )
where

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Ext.Data.Aeson as J
import GHC.Generics (Generic)

data UserSerializer = UserSerializer
  { usId :: Int,
    usCreatedAt :: Time.UTCTime,
    usPhone :: T.Text,
    usDateOfBirth :: Maybe Time.UTCTime,
    usAddress :: Maybe AddressSerializer
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON UserSerializer where
  toJSON = J.droppedPrefixDecode

data PersonalInfoSerializer = PersonalInfoSerializer
  { pisDateOfBirth :: Maybe Time.UTCTime,
    pisAddress :: AddressSerializer
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON PersonalInfoSerializer where
  toJSON = J.droppedPrefixDecode

instance J.FromJSON PersonalInfoSerializer where
  parseJSON = J.droppedPrefixParse

data AddressSerializer = AddressSerializer
  { asStreet :: Maybe T.Text,
    asCity :: Maybe T.Text,
    asZipCode :: Maybe T.Text
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON AddressSerializer where
  toJSON = J.droppedPrefixDecode

instance J.FromJSON AddressSerializer where
  parseJSON = J.droppedPrefixParse
