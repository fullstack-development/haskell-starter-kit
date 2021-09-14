module AppName.Gateways.Database.Connection
  ( createPgConnString,
  )
where

import qualified AppName.Config as Config
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (ConnectionString)

createPgConnString :: Config.AppConfig -> ConnectionString
createPgConnString config = do
  let Config.DbConfig {..} = Config.dbConfig config
   in encodeUtf8 $
        "postgresql://" <> user <> ":" <> password <> "@" <> host <> ":" <> pack (show port) <> "/" <> database
