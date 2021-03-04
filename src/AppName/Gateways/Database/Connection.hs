module AppName.Gateways.Database.Connection
  ( createPgConnString,
  )
where

import Config (Config)
import qualified Data.Configurator as C
import Database.Persist.Postgresql (ConnectionString)

createPgConnString :: Config -> IO ConnectionString
createPgConnString config = do
  host <- C.require config "database.host"
  name <- C.require config "database.name"
  user <- C.require config "database.user"
  pass <- C.require config "database.pass"
  port <- C.require config "database.port"
  let proto = "postgresql://"
  pure $
    proto <> user <> ":" <> pass <> "@" <> host <> ":" <> port <> "/" <> name
