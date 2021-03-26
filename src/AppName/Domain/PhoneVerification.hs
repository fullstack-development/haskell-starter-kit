{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module AppName.Domain.PhoneVerification
  ( Code,
    UncheckedPhone (..),
    CheckedPhone,
    Phone,
    Parameters (..),
    WaitConfirmationEntry (..),
    checkPhone,
    defParams,
    phoneToText,
    genConfirmationCode,
    codeToText,
    isConfirmReqExpired,
  )
where

import qualified Data.Aeson as J
import Data.Bool (bool)
import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime)
import System.Random (RandomGen, randomR)
import Text.Read (readMaybe)

type PhoneNumber = T.Text

newtype UncheckedPhone
  = UncheckedPhone PhoneNumber
  deriving (Eq, Show, Ord)

newtype CheckedPhone
  = CheckedPhone PhoneNumber
  deriving (Eq, Show, Ord)

type Phone = CheckedPhone

type Code = PhoneConfirmationCode

-- TODO: check phone on prefixes, digits and etc.
-- and implement via monoidal validation
checkPhone :: UncheckedPhone -> Either T.Text CheckedPhone
checkPhone (UncheckedPhone number) =
  let isAllDigits = all isDigit . drop 1 . T.unpack $ number
      checked = CheckedPhone number
   in bool (Left "Not digits") (Right checked) isAllDigits

phoneToText :: CheckedPhone -> PhoneNumber
phoneToText (CheckedPhone num) = num

newtype PhoneConfirmationCode
  = PhoneConfirmationCode [Int]
  deriving (Eq, Show)

instance J.FromJSON PhoneConfirmationCode where
  parseJSON (J.String raw) =
    let strs = (: []) <$> T.unpack raw
        digits = catMaybes $ readMaybe <$> strs
     in pure $ PhoneConfirmationCode digits
  parseJSON j = fail $ "Can't parse PhoneConfirmationCode from " <> show j

instance J.ToJSON PhoneConfirmationCode where
  toJSON = J.String . codeToText

codeToText :: PhoneConfirmationCode -> T.Text
codeToText (PhoneConfirmationCode digits) =
  T.intercalate "" . fmap (T.pack . show) $ digits

data Parameters = Parameters
  { pCodeLength :: Int,
    pCodeExpiration :: Seconds
  }
  deriving (Eq, Show)

newtype Seconds
  = Seconds Int
  deriving (Eq, Show, Ord, Num)

defParams :: Parameters
defParams = Parameters {pCodeLength = 4, pCodeExpiration = fiveMinutes}
  where
    fiveMinutes = 60 * 5

genConfirmationCode ::
  RandomGen g => CheckedPhone -> Parameters -> g -> (PhoneConfirmationCode, g)
genConfirmationCode _ Parameters {..} = loop pCodeLength []
  where
    loop len list gen
      | len > 0 =
        let !(!digit, !gen') = randomR (0, 9) gen
         in loop (pred len) (digit : list) gen'
      | otherwise = (PhoneConfirmationCode list, gen)

data WaitConfirmationEntry
  = WaitConfirmationEntry
      Phone
      Code
      UTCTime
  deriving (Eq, Show)

instance J.ToJSON WaitConfirmationEntry where
  toJSON (WaitConfirmationEntry phone code time) =
    J.object
      ["phone" J..= phoneToText phone, "code" J..= code, "time" J..= time]

instance J.FromJSON WaitConfirmationEntry where
  parseJSON =
    J.withObject "WaitConfirmationEntry" $ \o -> do
      dirtyPhone <- o J..: "phone"
      let eiPhone = checkPhone $ UncheckedPhone dirtyPhone
      phone <- either (fail . T.unpack) pure eiPhone
      code <- o J..: "code"
      time <- o J..: "time"
      pure $ WaitConfirmationEntry phone code time

isConfirmReqExpired :: UTCTime -> WaitConfirmationEntry -> Bool
isConfirmReqExpired time (WaitConfirmationEntry _ _ createdAt) =
  let fiveMinutes = 60 * 5
   in diffUTCTime time createdAt > fiveMinutes
